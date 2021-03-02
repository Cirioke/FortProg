# FortProg
Praktikum

Ein simpler Proolog-Interpreter in Haskell ohne Modulfunktion.

----------------------
Um dem ghc/ghci alternative Pfade zum überpüfen nach Modulen zu geben:

    $> ghci -i"<pfad1>:<pfad2>…<pfad3>"
    $> ghc -i"<pfad1>:<pfad2>…<pfad3>" main.hs

um sich schreibkram zu sparen lässt sich z.B. eine Umgebungsvariable HASKELL_PATH anzulegen, welche alle Pfade enthält z.B.:

<Path to FortProg>/Modules:
<Path to FortProg>/Modules/InteraktiveEnvironment/scr:
<Path to FortProg>/Modules/InteraktiveEnvironment/tests:
<Path to FortProg>/Modules/PrettyPrint/scr:
<Path to FortProg>/Modules/PrettyPrint/tests:
<Path to FortProg>/Modules/Renaming/scr:
<Path to FortProg>/Modules/Renaming/tests:
<Path to FortProg>/Modules/SLDResolution/scr:
<Path to FortProg>/Modules/SLDResolution/tests:
<Path to FortProg>/Modules/Substitutions/scr:
<Path to FortProg>/Modules/Substitutions/tests:
<Path to FortProg>/Modules/Unification/scr:
<Path to FortProg>/Modules/Unification/tests:
<Path to FortProg>/Modules/Variables/scr:
<Path to FortProg>/Modules/Variables/tests:


Damit lässt sich dann dann einfach Compilieren:

# Bash

    $> ghc -i"$HASKELL_PATH" Main.hs

# PowerShell

    $> ghc -i"$env:HASKELL_PATH" Main.hs