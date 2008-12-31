dopath("goto_multihead")

defbindings("WScreen", {
    -- Bind some function keys under alt, since other apps use the plain ones.
    kpress(ALTMETA.."F12", nil),
    kpress(META.."F12", "mod_query.query_menu(_, 'mainmenu', 'Main menu: ')"),
    kpress(META.."F11", "ioncore.exec_on(_, 'xscreensaver-command -lock')"),

    kpress(META.."KP_Divide", "WScreen.switch_prev(_)"),
    kpress(META.."KP_Multiply", "WScreen.switch_next(_)"),

    bdoc("Go to next screen on multihead setup."),
    kpress(META.."KP_Insert", "ioncore.goto_next_screen()"),

    -- I need easy switching between screens, switching between virtual
    -- desktops in META+number. 
    bdoc("Go to next/previous screen on multihead setup."),
    kpress(META.."comma", "ioncore.goto_prev_screen()"),
    kpress(META.."period", "ioncore.goto_next_screen()"),

    bdoc("Switch to next/previous object within current screen."),
    kpress(META.."Shift+comma", "WScreen.switch_prev(_)"),
    kpress(META.."Shift+period", "WScreen.switch_next(_)"),

    kpress(META.."BackSpace", "ioncore.goto_previous()"),
})

-- Keypad navigation in frames
defbindings("WTiling", {
    bdoc("Go to frame above/below/right/left of current frame."),

    kpress(META.."KP_Left", "goto_multihead.goto_next(_sub, 'left')"),
    kpress(META.."KP_Right", "goto_multihead.goto_next(_sub, 'right')"),
    kpress(META.."KP_Up", "goto_multihead.goto_next(_sub, 'up')"),
    kpress(META.."KP_Down", "goto_multihead.goto_next(_sub, 'down')"),

    kpress(META.."Left", "goto_multihead.goto_next(_sub, 'left', {no_ascend=_})"),
    kpress(META.."Right", "goto_multihead.goto_next(_sub, 'right', {no_ascend=_})"),
    kpress(META.."Up", "goto_multihead.goto_next(_sub, 'up', {no_ascend=_})"),
    kpress(META.."Down", "goto_multihead.goto_next(_sub, 'down', {no_ascend=_})"),
})

defbindings("WFrame", {
    bdoc("Switch to next/previous object within the frame."),
    kpress(META.."Prior", "WFrame.switch_prev(_)"),
    kpress(META.."Next", "WFrame.switch_next(_)"),
    kpress(META.."Tab", "WFrame.switch_next(_)"),
    kpress(META.."Shift+Tab", "WFrame.switch_prev(_)"),

    bdoc("Switch to nth object within the frame."),
    kpress(ALTMETA.."1", "WFrame.switch_nth(_, 0)"),
    kpress(ALTMETA.."2", "WFrame.switch_nth(_, 1)"),
    kpress(ALTMETA.."3", "WFrame.switch_nth(_, 2)"),
    kpress(ALTMETA.."4", "WFrame.switch_nth(_, 3)"),
    kpress(ALTMETA.."5", "WFrame.switch_nth(_, 4)"),
    kpress(ALTMETA.."6", "WFrame.switch_nth(_, 5)"),
    kpress(ALTMETA.."7", "WFrame.switch_nth(_, 6)"),
    kpress(ALTMETA.."8", "WFrame.switch_nth(_, 7)"),
    kpress(ALTMETA.."9", "WFrame.switch_nth(_, 8)"),
    kpress(ALTMETA.."0", "WFrame.switch_nth(_, 9)"),
})

defbindings("WMPlex.toplevel", {
    -- Bind some function keys under alt, since other apps use the plain ones.
    kpress(ALTMETA.."F1", nil),
    kpress(META.."F1", "mod_query.query_man(_, ':man')"),
    kpress(ALTMETA.."F2", nil),
    kpress(META.."F2", "ioncore.exec_on(_, 'x-terminal-emulator')"),
    kpress(ALTMETA.."F3", nil),
    kpress(META.."F3", "mod_query.query_exec(_)"),
    kpress(ALTMETA.."F4", nil), 
    kpress(ALTMETA.."F5", nil),
    kpress(ALTMETA.."F6", nil),
    kpress(META.."F6", "mod_query.query_runfile(_, 'run-mailcap --action=view')"),
    kpress(ALTMETA.."F9", nil),
    kpress(META.."F9", "mod_query.query_workspace(_)"),

    kpress("XF86WWW", "ioncore.exec_on(_, 'x-www-browser')"),
    kpress("XF86Search", "ioncore.exec_on(_, 'xfe')"),
    kpress("XF86Mail", "ioncore.exec_on(_, 'x-terminal-emulator')"),

    kpress("XF86Launch1", "ioncore.exec_on(_, 'dcop amarok player setRating 2')"),
    kpress("XF86Launch2", "ioncore.exec_on(_, 'dcop amarok player setRating 4')"),
    kpress("XF86Launch3", "ioncore.exec_on(_, 'dcop amarok player setRating 6')"),
    kpress("XF86Launch4", "ioncore.exec_on(_, 'dcop amarok player setRating 8')"),
    kpress("XF86Launch5", "ioncore.exec_on(_, 'dcop amarok player setRating 10')"),

    kpress("XF86AudioPlay", "ioncore.exec_on(_, 'dcop amarok player pausePlay')"),
    kpress("XF86AudioPrev", "ioncore.exec_on(_, 'dcop amarok player prev')"),
    kpress("XF86AudioNext", "ioncore.exec_on(_, 'dcop amarok player next')"),

    -- kpress("XF86Back", "WScreen.switch_prev(_)"),
    -- kpress("XF86Forward", "WScreen.switch_next(_)"),
})

-- Single-frame layout
ioncore.deflayout("default", ioncore.getlayout("full"))
