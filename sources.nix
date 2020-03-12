let
  excludeDirs = [ ".git" "output" ".spago" "dist" "dist-newstyle" ];
in
builtins.filterSource
  (path: type: type != "directory" || ! builtins.elem (baseNameOf path) excludeDirs)
  ./.
