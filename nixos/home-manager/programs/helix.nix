{ ... }:

{
  programs.helix = {
    enable = true;
    # TODO: Wait for this to become stable.
    # defaultEditor = true;

    languages.language = [{
      name = "rust";
      auto-format = true;
    }];

    settings = {
      theme = "autumn_night";
      editor.true-color = true;  # It thinks color support doesn't exist otherwise.

      keys.normal = {
        n = "move_visual_line_down";
        N = "join_selections";
        e = "move_visual_line_up";
        k = "search_next";
        K = "search_prev";
        j = "move_next_word_end";
        J = "move_next_long_word_end";
        ";" = "command_mode";
        ":" = "collapse_selection";
      };

      keys.select = {
        n = "extend_visual_line_down";
        e = "extend_visual_line_up";
        k = "extend_search_next";
        K = "extend_search_prev";
      };
    };
  };
}
