$path=""
$base_path_win = Get-Location
$base_path_nix = ($base_path_win -replace "\\","/").Trim("/")
foreach($line in Get-Content .\HASKELL_PATH.txt) {
  $path = "$path"+":$base_path_nix$line"
}
$path