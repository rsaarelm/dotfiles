{ ... }:

{
  programs.helix = {
    enable = true;
    # TODO Activate defaultEditor flag when it becomes stable.
    # defaultEditor = true;

    languages.language = [{
      name = "rust";
      auto-format = true;
    }];

    settings = {
      theme = "autumn_night";
      editor.true-color =
        true; # It thinks color support doesn't exist otherwise.

      keys.normal = {
        n = "move_visual_line_down";
        N = "join_selections";
        e = "move_visual_line_up";
        k = "search_next";
        K = "search_prev";
        f = "move_next_word_end";
        F = "move_next_long_word_end";
        j = "find_next_char";
        J = "find_prev_char";
        ";" = "command_mode";
        ":" = "collapse_selection";

        # More agile window movement
        "C-h" = "jump_view_left";
        "C-l" = "jump_view_right";
        "C-n" = "jump_view_down";
        "C-e" = "jump_view_up";
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
