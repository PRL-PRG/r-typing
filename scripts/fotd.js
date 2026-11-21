// The function of the day: the GitHub side, for actions/github-script.
//
// - daily(): .github/workflows/function-of-the-day.yml. Opens an issue about
//   an R function the deployed dashboard types in both modes (its fotd.json),
//   one never featured before, and closes the previous ones that only got
//   thumbs up (approvedByReactions).
// - writeFeatured() / postChanges(): the Pipeline (ci.yml). Lists the
//   functions featured so far, for scripts/fotd_changes.R, then comments the
//   changes it finds on their issues and reopens them.
//
// The settings (assignees, labels, the dashboard's URL) are in
// .github/function-of-the-day.yml. An issue names its function in a hidden
// marker of its body, <!-- fotd: pkg::name -->: the issues are the history.

const fs = require('fs');

const CONFIG = '.github/function-of-the-day.yml';
const MARKER = /<!-- fotd: ([^\s:]+)::(\S+?) -->/;

// The configuration file: `key: value` lines and `key:` followed by `- item`
// lines. Comments start with #.
function readConfig(path = CONFIG) {
  const config = {};
  let list = null;
  for (const raw of fs.readFileSync(path, 'utf8').split('\n')) {
    const line = raw.replace(/(^|\s)#.*$/, '').trimEnd();
    if (!line.trim()) continue;
    const item = line.match(/^\s+-\s*(.+)$/);
    if (item && list) { list.push(item[1].trim()); continue; }
    const kv = line.match(/^([A-Za-z_]+):\s*(.*)$/);
    if (!kv) throw new Error(`${path}: cannot read "${raw}"`);
    if (kv[2]) { config[kv[1]] = kv[2].trim(); list = null; }
    else { list = config[kv[1]] = []; }
  }
  config.assignees ??= [];
  config.labels ??= [];
  config.label ??= 'function-of-the-day';
  config.changed_label ??= 'type-changed';
  if (!config.dashboard) throw new Error(`${path}: no dashboard URL`);
  config.dashboard = config.dashboard.replace(/\/*$/, '/');
  return config;
}

// The issues of the functions of the day, open or closed, oldest first.
async function featuredIssues(github, context, config) {
  const issues = await github.paginate(github.rest.issues.listForRepo, {
    owner: context.repo.owner, repo: context.repo.repo,
    labels: config.label, state: 'all', per_page: 100,
  });
  return issues
    .filter(i => !i.pull_request)
    .map(i => {
      const m = (i.body || '').match(MARKER);
      return m && { issue: i.number, state: i.state, created_at: i.created_at,
                    labels: i.labels.map(l => l.name), reactions: i.reactions,
                    package: m[1], name: m[2] };
    })
    .filter(Boolean)
    .sort((a, b) => a.issue - b.issue);
}

async function ensureLabel(github, context, name, color, description) {
  try {
    await github.rest.issues.createLabel({ owner: context.repo.owner, repo: context.repo.repo,
                                           name, color, description });
  } catch (e) {
    if (e.status !== 422) throw e;   // 422: it exists
  }
}

// A fence longer than any run of backticks in the text.
function fenced(text, lang = '') {
  const longest = Math.max(2, ...(text.match(/`+/g) || []).map(s => s.length));
  const f = '`'.repeat(longest + 1);
  return `${f}${lang}\n${text}\n${f}`;
}

// A type on several lines when it does not fit in [width] columns: an outer
// (...) or { ... } is entered, then the type is broken before its operators
// of the lowest precedence at that level (where and and of a recursive type,
// & between overloads, then |, then ->), or else after the commas between
// parameters or fields, each part indented and broken in turn. Only spaces
// and line breaks are added. A scheme's quantifier (∀'a,'b.) goes on its own
// line.
const TYPE_WIDTH = 90;
const TYPE_OPS = [' where ', ' and ', ' & ', ' | ', ' -> ', ' --> '];

// The top-level positions of [op] in [s]: outside brackets, classes and
// quoted names.
function topLevel(s, op) {
  const found = [];
  let depth = 0, quoted = false;
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if (quoted) { if (c === '\\') i++; else if (c === '"') quoted = false; continue; }
    if (c === '"') { quoted = true; continue; }
    if ('([{'.includes(c) || (c === '<' && s[i + 1] !== '=')) depth++;
    else if (')]}'.includes(c) || (c === '>' && s[i - 1] !== '-' && depth > 0)) depth--;
    else if (depth === 0 && s.startsWith(op, i)) found.push(i);
  }
  return found;
}

// [s] is (inner)suffix or { inner }suffix, the bracket at 0 closing just
// before suffix (classes, possibly with attributes, or [?]).
function outerGroup(s) {
  const open = s[0], close = { '(': ')', '{': '}' }[open];
  if (!close) return null;
  let depth = 0, quoted = false;
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if (quoted) { if (c === '\\') i++; else if (c === '"') quoted = false; continue; }
    if (c === '"') quoted = true;
    else if (c === '(' || c === '{') depth++;
    else if ((c === ')' || c === '}') && --depth === 0) {
      if (c !== close) return null;
      const suffix = s.slice(i + 1);
      if (!/^(<[^(){}]*>)?\??$/.test(suffix)) return null;
      const inner = s.slice(1, i);
      return open === '{' ? { open: '{ ', inner: inner.trim(), close: ' }', suffix }
                          : { open: '(', inner, close: ')', suffix };
    }
  }
  return null;
}

function breakType(s, indent, width = TYPE_WIDTH) {
  if (indent + s.length <= width) return s;
  const outer = outerGroup(s);
  if (outer) {
    const inner = breakType(outer.inner, indent + outer.open.length, width);
    return `${outer.open}${inner}${outer.close}${outer.suffix}`;
  }
  for (const op of TYPE_OPS) {
    const at = topLevel(s, op);
    if (at.length === 0) continue;
    const parts = [];
    let start = 0;
    for (const i of at) { parts.push(s.slice(start, i)); start = i + op.length; }
    parts.push(s.slice(start));
    const sym = op.trim();
    const pad = ' '.repeat(indent);
    return parts.map((p, k) => k === 0
      ? breakType(p, indent, width)
      : `\n${pad}${sym} ${breakType(p, indent + sym.length + 1, width)}`).join('');
  }
  const commas = topLevel(s, ', ');
  if (commas.length > 0) {
    const parts = [];
    let start = 0;
    for (const i of commas) { parts.push(s.slice(start, i)); start = i + 2; }
    parts.push(s.slice(start));
    const pad = ' '.repeat(indent);
    return parts.map(p => breakType(p, indent, width)).join(`,\n${pad}`);
  }
  return s;
}

function formatType(t, width = TYPE_WIDTH) {
  const m = t.match(/^(∀[^.]*\.)\s*(.*)$/s);
  return m ? `${m[1]}\n${breakType(m[2], 0, width)}` : breakType(t, 0, width);
}

// The lines of [a] and [b] as a unified diff without headers (' ', '-', '+'
// prefixes), from their longest common subsequence: GitHub colours a ```diff
// block by line.
function lineDiff(a, b) {
  const x = a.split('\n'), y = b.split('\n');
  const lcs = Array.from({ length: x.length + 1 }, () => new Array(y.length + 1).fill(0));
  for (let i = x.length - 1; i >= 0; i--)
    for (let j = y.length - 1; j >= 0; j--)
      lcs[i][j] = x[i] === y[j] ? lcs[i + 1][j + 1] + 1 : Math.max(lcs[i + 1][j], lcs[i][j + 1]);
  const out = [];
  let i = 0, j = 0;
  while (i < x.length || j < y.length) {
    if (i < x.length && j < y.length && x[i] === y[j]) { out.push(` ${x[i]}`); i++; j++; }
    else if (i < x.length && (j === y.length || lcs[i + 1][j] >= lcs[i][j + 1])) out.push(`-${x[i++]}`);
    else out.push(`+${y[j++]}`);
  }
  return out.join('\n');
}

// The diff of two types, broken more finely than for display (GitHub does
// not highlight within a line).
const DIFF_WIDTH = 60;
function typeDiff(was, now) {
  return lineDiff(formatType(was, DIFF_WIDTH), formatType(now, DIFF_WIDTH));
}

function secs(x) {
  if (x == null || isNaN(x)) return '?';
  return x < 0.01 ? '<0.01 s' : `${x.toFixed(2)} s`;
}

// GitHub refuses an issue body over 65536 characters.
const MAX_BODY = 65000;

// The native functions the R function calls: their type, and their source
// unless `withSource` is false.
function nativesSection(f, withSource) {
  const natives = f.natives || [];
  if (natives.length === 0) return [];
  const out = ['', `### Native functions it calls`];
  for (const n of natives) {
    const where = n.file ? ` (\`${n.file}\`${n.line ? `, line ${n.line}` : ''})` : '';
    const alias = n.r_name && n.r_name !== n.name ? `, called from R as \`${n.r_name}\`` : '';
    out.push('', `#### \`${n.name}\`${where}${alias}`);
    if (n.type) out.push('Typed:', fenced(n.type));
    else if (n.status) out.push(`Not typed (${n.status}).`);
    else out.push('Not among the analysed C functions of the package.');
    if (withSource && n.source) out.push('<details><summary>Source</summary>', '', fenced(n.source, n.lang || 'c'), '', '</details>');
  }
  if (!withSource && natives.some(n => n.source))
    out.push('', `*Their sources are left out to keep this issue under GitHub's size limit; they are on the package's page.*`);
  return out;
}

function issueBody(f, data, config) {
  const body = issueBodyWith(f, data, config, true);
  return body.length <= MAX_BODY ? body : issueBodyWith(f, data, config, false);
}

function issueBodyWith(f, data, config, nativeSources) {
  const card = `${config.dashboard}${f.page}`;
  const pkgPage = `${config.dashboard}pkg/${f.package}.html`;
  const version = f.version ? ` ${f.version}` : '';
  const checker = data.checker_version ? ` \`${data.checker_version}\`` : '';
  const run = data.run_url ? ` in [this run](${data.run_url})` : '';
  return [
    `<!-- fotd: ${f.package}::${f.name} -->`,
    `\`${f.name}\`, from the package **${f.package}**${version} (\`${f.file}\`, line ${f.line}), typed by tyr${checker}${run}.`,
    '',
    `[Its card on the dashboard](${card}) · [the package's page](${pkgPage}) · [the dashboard](${config.dashboard})`,
    '',
    `### Strict mode`,
    `Typed in ${secs(f.strict.timing_sec)}:`,
    fenced(formatType(f.strict.type)),
    '',
    `### Gradual mode`,
    ...(f.gradual.type === f.strict.type
      ? [`Typed in ${secs(f.gradual.timing_sec)}, to the same type.`]
      : [`Typed in ${secs(f.gradual.timing_sec)}:`, fenced(formatType(f.gradual.type))]),
    '',
    `### Source`,
    fenced(f.source, 'r'),
    ...nativesSection(f, nativeSources),
    '',
    '---',
    '**Is this a good type for this function?** React to this issue with 👍 or 👎, and comment to say why: too wide, too narrow, wrong, or what you would expect.',
    '',
    `*With only 👍, this issue is closed automatically the next morning; with any other reaction, it stays open for someone to close by hand. A later run in which this function's type changes, or it stops typing, comments here and reopens the issue (label \`${config.changed_label}\`).*`,
  ].join('\n');
}

// Opens today's issue, unless one was already opened today (a rerun) and
// `force` is false.
async function daily({ github, context, core, force = false }) {
  const config = readConfig();
  const res = await fetch(`${config.dashboard}fotd.json`);
  if (!res.ok) throw new Error(`${config.dashboard}fotd.json: HTTP ${res.status}`);
  const data = await res.json();

  const featured = await featuredIssues(github, context, config);
  const today = new Date().toISOString().slice(0, 10);
  if (!force && featured.some(f => f.created_at.slice(0, 10) === today)) {
    core.notice('A function of the day was already opened today.');
    return;
  }
  const used = new Set(featured.map(f => `${f.package}::${f.name}`));
  const candidates = (data.functions || [])
    .filter(f => !/\s/.test(f.name) && !used.has(`${f.package}::${f.name}`));
  if (candidates.length === 0) {
    core.warning('Every typed function has been featured already.');
    return;
  }
  const f = candidates[Math.floor(Math.random() * candidates.length)];

  const pkgLabel = `pkg:${f.package}`;
  await ensureLabel(github, context, config.label, '0e8a16', 'An R function the dashboard types, featured for the day');
  await ensureLabel(github, context, config.changed_label, 'd93f0b', "The featured function's type changed since");
  await ensureLabel(github, context, pkgLabel, 'c5def5', `The ${f.package} package`);
  for (const l of config.labels) await ensureLabel(github, context, l, 'ededed', '');

  const { data: issue } = await github.rest.issues.create({
    owner: context.repo.owner, repo: context.repo.repo,
    title: `Function of the day: ${f.package}::${f.name}`,
    body: issueBody(f, data, config),
    labels: [config.label, pkgLabel, ...config.labels],
  });
  core.notice(`Opened #${issue.number} about ${f.package}::${f.name} (one of ${candidates.length} candidates).`);

  // Assigned one at a time, after the issue is opened, so that a login GitHub
  // refuses leaves out only that login. No checkUserCanBeAssigned beforehand:
  // with the workflow's token, it answers 404 for collaborators whose
  // membership of the organization is private.
  for (const login of config.assignees) {
    try {
      const { data: updated } = await github.rest.issues.addAssignees({
        owner: context.repo.owner, repo: context.repo.repo,
        issue_number: issue.number, assignees: [login],
      });
      // GitHub ignores, without an error, a login it cannot assign.
      if (!updated.assignees.some(a => a.login.toLowerCase() === login.toLowerCase()))
        core.warning(`${login} was not assigned to #${issue.number}: GitHub ignored it.`);
    } catch (e) {
      core.warning(`${login} could not be assigned to #${issue.number}: ${e.message}`);
    }
  }

  // The previous days' issues that only got thumbs up, unless a change
  // reopened them: the others are left for a human to close.
  let closed = 0;
  for (const old of featured) {
    if (old.state === 'open' && !old.labels.includes(config.changed_label)
        && approvedByReactions(old.reactions)) {
      closed++;
      await github.rest.issues.update({ owner: context.repo.owner, repo: context.repo.repo,
                                        issue_number: old.issue, state: 'closed', state_reason: 'completed' });
    }
  }
  const left = featured.filter(f => f.state === 'open').length - closed;
  if (left > 0) core.notice(`${left} previous issue(s) left open: not only thumbs up, or reopened by a change.`);
}

// Whether an issue's reactions approve its type: at least one 👍, and no
// other reaction (👎, or any other emoji, asks for a human to close it). An
// issue nobody reacted to stays open too.
function approvedByReactions(reactions) {
  if (!reactions) return false;
  const up = reactions['+1'] || 0;
  return up > 0 && (reactions.total_count || 0) === up;
}

// featured.json for scripts/fotd_changes.R.
async function writeFeatured({ github, context, core, path }) {
  const config = readConfig();
  const featured = await featuredIssues(github, context, config);
  fs.writeFileSync(path, JSON.stringify(featured.map(({ issue, package: p, name }) => ({ issue, package: p, name }))));
  core.info(`${featured.length} functions of the day.`);
}

// Comments the changes scripts/fotd_changes.R found, and reopens the issues.
async function postChanges({ github, context, core, path }) {
  const config = readConfig();
  const out = JSON.parse(fs.readFileSync(path, 'utf8'));
  if (out.broken) { core.warning(`Functions of the day: ${out.reason}`); return; }
  const featured = new Map((await featuredIssues(github, context, config)).map(f => [f.issue, f]));
  await ensureLabel(github, context, config.changed_label, 'd93f0b', "The featured function's type changed since");
  for (const c of out.changes) {
    const params = { owner: context.repo.owner, repo: context.repo.repo, issue_number: c.issue };
    // The types scripts/fotd_changes.R tagged, on several lines.
    const body = c.body
      .replace(/```type\n([^\n]*)\n```/g, (_, t) => fenced(formatType(t)))
      .replace(/```typediff\n([^\n]*)\n([^\n]*)\n```/g, (_, was, now) => fenced(typeDiff(was, now), 'diff'));
    await github.rest.issues.createComment({ ...params, body });
    await github.rest.issues.addLabels({ ...params, labels: [config.changed_label] });
    if (featured.get(c.issue)?.state === 'closed')
      await github.rest.issues.update({ ...params, state: 'open' });
  }
  core.notice(`Functions of the day: ${out.changes.length} changed.`);
}

module.exports = { readConfig, daily, writeFeatured, postChanges, issueBody, formatType, typeDiff, approvedByReactions };
