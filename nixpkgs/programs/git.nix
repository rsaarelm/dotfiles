{ ... }:

{
  programs.git = {
    enable = true;
    userName = "rsaarelm";
    userEmail = "risto.saarelma@iki.fi";

    aliases = {
      ap = "add --patch";
      # Terse log of current branch
      #
      # The default version only shows the recent commits for rebase ops on the
      # same terminal session
      lg =
        "log --color=always --pretty='[%C(cyan)%h%Creset]%C(bold cyan)%d%Creset %s %C(bold blue)(%cr)%Creset %C(blue)<%an>%Creset' -n 10";
      lgg =
        "log --graph --color=always --pretty='[%C(cyan)%h%Creset]%C(bold cyan)%d%Creset %s %C(bold blue)(%cr)%Creset %C(blue)<%an>%Creset'";
      # The full version
      lga =
        "log --color=always --pretty='[%C(cyan)%h%Creset]%C(bold cyan)%d%Creset %s %C(bold blue)(%cr)%Creset %C(blue)<%an>%Creset'";
      # Logs of all branches
      hist =
        "log --all --graph --color=always --pretty='[%C(cyan)%h%Creset]%C(bold cyan)%d%Creset %s %C(bold blue)(%ci)%Creset %C(blue)<%an>%Creset'";
      # Fap
      fap = "fetch --all --prune";
      # Short-form status
      s = "status -sb";
      # Show diff in index but not in working tree
      ix = "diff --cached";
      # Show diff of both index and working tree
      d = "diff HEAD";
      # Set date of top commit to now
      redate = "commit --amend --no-edit --date='now'";
      # Amend commit, keep message
      cam = "commit --amend --no-edit --allow-empty-message";
      # Update submodules
      subu = "submodule update --recursive";
      # Rebase-pull
      purr = "pull --rebase";
      # Make a messageless snapshot commit when using git as a backup system
      snap = "commit -am .";
      # Commit with inline message
      cm = "commit -m";
      # Non-annoying merging
      mer = "merge --ff-only";
      pul = "pull --ff-only";
      # Words
      wd = "diff --word-diff";
      wshow = "show --word-diff";
      # Safer force
      fpush = "push --force-with-lease";
      # Clone without history
      fastclone = "clone --depth 1";
    };

    extraConfig = {
      color.ui = "true";
      rebase.autosquash = "true";
      diff.wsErrorHighlight = "all";
      branch.sort = "-authordate";
    };
  };
}
