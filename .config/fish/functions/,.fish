# A better version of the NixOS comma command

# Better as in, doesn't require you to mess with the nixpkgs data file,
# happily runs unfree stuff and has preferred options hardcoded so there's no
# interactive prompt slowing you down.
function ','
    set -l cmd $argv[1]
    set -l args (string escape -- $argv[2..-1])

    set -l pkg $cmd
    switch $cmd
        case Discord; set pkg discord
        case FBReader; set pkg fbreader
        case btm; set pkg bottom
        case getgbook; set pkg getxbook
        case glxgears; set pkg glxinfo
        case grafx2-sdl; set pkg grafx2
        case keeper; set pkg keeperrl
        case perf; set pkg linuxPackages.perf
        case wtfutil; set pkg wtf
        case xev; set pkg xorg.xev
        case yt; set pkg yewtube
    end
    NIXPKGS_ALLOW_UNFREE=1 nix-shell -p $pkg --run "$cmd $args"
end
