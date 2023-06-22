{ ... }:

{
  programs.helix = {
    enable = true;

    languages.language = [{
      name = "rust";
      auto-format = true;
    }];

    settings = {
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
