# Setting up git for this folder

Git was not available on the machine used to prepare this folder (only
GitHub CLI, `gh.exe`, was found — `gh` alone cannot initialize/commit a repo,
it still needs `git` underneath). Install Git for Windows
(<https://git-scm.com/download/win>) or GitHub Desktop, then from **this
folder** (`PEP_QC_TCAMS/`) run:

```powershell
git init
git add .
git commit -m "Reorganize TCAMS as a standalone repo; fix species display names, HDI default, posterior-draws path bug; add Trajectories tab"

git remote add origin https://github.com/joaoczarnecki/Tree-Community-Assisted-Migration-Simulator_TCAMS.git

# The existing GitHub repo may already have commit history from earlier
# manual deployments. Check before force-pushing:
git fetch origin
git log --oneline origin/main   # or origin/master — see what's there

# If the remote is empty or you're OK replacing its history with this
# clean snapshot:
git push -u origin main --force

# If instead you want to preserve remote history and just add this as a
# new commit on top, pull/rebase first:
# git pull origin main --allow-unrelated-histories
# git push -u origin main
```

**Before force-pushing**, please confirm with the repo owner (you) that
nothing on the remote is needed — the `--force` push will overwrite
whatever is currently published at that URL.

## Repository size check

Run this after `git add .` and before committing, to make sure no single
file exceeds GitHub's 100 MB hard limit (everything here was kept under
~22 MB per file on purpose):

```powershell
git ls-files -s | ForEach-Object {
  $parts = $_ -split '\t'
  $path = $parts[1]
  if (Test-Path $path) {
    $sizeMB = [math]::Round((Get-Item $path).Length / 1MB, 1)
    if ($sizeMB -gt 50) { "$sizeMB MB`t$path" }
  }
} | Sort-Object -Descending
```
