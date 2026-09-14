# Git setup for this folder

Git is now installed as a **portable MinGit** at
`%LOCALAPPDATA%\Tools\MinGit\cmd\git.exe` (added to your **User PATH**).
**Open a brand-new terminal/VS Code window** for the PATH change to take
effect — then plain `git` commands work without a full path.

This repo has already been initialized, committed locally, and had its
`origin` remote pointed at
<https://github.com/joaoczarnecki/Tree-Community-Assisted-Migration-Simulator_TCAMS.git>.
Nothing has been pushed yet — that step is left for you to run deliberately,
since it can overwrite what's currently published there.

## Push to GitHub

```powershell
# See what's already on the remote before deciding how to push:
git fetch origin
git log --oneline origin/main   # or origin/master — see what history exists there

# Option A — the remote is empty, or you're OK replacing its history with
# this clean, safeguarded snapshot:
git push -u origin main --force

# Option B — you want to preserve the remote's existing history and layer
# this snapshot on top instead:
# git pull origin main --allow-unrelated-histories
# git push -u origin main
```

**Before force-pushing**, confirm nothing on the remote is needed — `--force`
overwrites whatever is currently published at that URL. The remote's history
is the only place the previous shinyapps.io deployment metadata
(`old/CAMS_Shiny_App_PA_Advanced/rsconnect/...`) is referenced from, so it's
worth a quick look with Option A's `git log` first.

## Repository size check

Already verified during this reorganization: total repo size ≈428 MB,
largest single file ≈21 MB — comfortably under GitHub's 100 MB per-file
hard limit, no Git LFS needed. Re-run this check if you add more data later:

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

## Notes specific to this session's environment

- A stray double-quote had corrupted the `HOME` **User** environment
  variable (`H:"` instead of `H:\`), which made every `git` invocation fail
  with `fatal: unable to access '...': Invalid argument`. This was fixed at
  the registry level (`H:\`, your existing network home drive, was
  preserved — not replaced with `C:\Users\...`). If you see that error again
  in a *new* terminal, check
  `[Environment]::GetEnvironmentVariable("HOME","User")` in PowerShell.
- `G:\` and other mapped/network drives can trigger git's "detected dubious
  ownership" safety check. This repo's path was already added as an
  exception via `git config --global --add safe.directory
  G:/Thesis/3rdChapter/PEP_QC_TCAMS`. If you move the repo to a different
  path, re-run that command with the new path.
- You may see `LF will be replaced by CRLF` warnings on commit — harmless,
  just Git normalizing line endings on Windows (`core.autocrlf=true` is set
  globally).
