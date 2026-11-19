// The function of the day: the GitHub side, for actions/github-script.
//
// - daily(): .github/workflows/function-of-the-day.yml. Opens an issue about
//   an R function the deployed dashboard types in both modes (its fotd.json),
//   one never featured before, and closes the previous ones.
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
                    labels: i.labels.map(l => l.name), package: m[1], name: m[2] };
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

function secs(x) {
  if (x == null || isNaN(x)) return '?';
  return x < 0.01 ? '<0.01 s' : `${x.toFixed(2)} s`;
}

function issueBody(f, data, config) {
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
    fenced(f.strict.type),
    '',
    `### Gradual mode`,
    ...(f.gradual.type === f.strict.type
      ? [`Typed in ${secs(f.gradual.timing_sec)}, to the same type.`]
      : [`Typed in ${secs(f.gradual.timing_sec)}:`, fenced(f.gradual.type)]),
    '',
    `### Source`,
    fenced(f.source, 'r'),
    '',
    '---',
    '**Is this a good type for this function?** React to this issue with 👍 or 👎, and comment to say why: too wide, too narrow, wrong, or what you would expect.',
    '',
    `*A later run in which this function's type changes, or it stops typing, comments here and reopens the issue (label \`${config.changed_label}\`).*`,
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

  // Only those GitHub accepts (collaborators of the repository): another
  // login would fail the whole request.
  const assignees = [];
  for (const login of config.assignees) {
    try {
      await github.rest.issues.checkUserCanBeAssigned({ owner: context.repo.owner, repo: context.repo.repo, assignee: login });
      assignees.push(login);
    } catch (e) {
      if (e.status !== 404) throw e;
      core.warning(`${login} cannot be assigned issues in this repository (not a collaborator?): left out.`);
    }
  }

  const { data: issue } = await github.rest.issues.create({
    owner: context.repo.owner, repo: context.repo.repo,
    title: `Function of the day: ${f.package}::${f.name}`,
    body: issueBody(f, data, config),
    labels: [config.label, pkgLabel, ...config.labels],
    assignees,
  });
  core.notice(`Opened #${issue.number} about ${f.package}::${f.name} (one of ${candidates.length} candidates).`);

  // The previous days' issues, unless a change reopened them.
  for (const old of featured) {
    if (old.state === 'open' && !old.labels.includes(config.changed_label)) {
      await github.rest.issues.update({ owner: context.repo.owner, repo: context.repo.repo,
                                        issue_number: old.issue, state: 'closed', state_reason: 'completed' });
    }
  }
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
    await github.rest.issues.createComment({ ...params, body: c.body });
    await github.rest.issues.addLabels({ ...params, labels: [config.changed_label] });
    if (featured.get(c.issue)?.state === 'closed')
      await github.rest.issues.update({ ...params, state: 'open' });
  }
  core.notice(`Functions of the day: ${out.changes.length} changed.`);
}

module.exports = { readConfig, daily, writeFeatured, postChanges, issueBody };
