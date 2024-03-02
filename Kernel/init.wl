(* ::Package:: *)

$m2path = Automatic;

InterfaceM2::mdosm2 = "Missing both Macaulay2 and WSL (Windows Subsystem for Linux). \
Please check https://aka.ms/wslinstall on how to setup WSL. Alternatively, define $M2Path.";
InterfaceM2::mwslm2 = "Missing Macaulay2 on your default WSL distro. \
Please check http://www2.macaulay2.com/Macaulay2/Downloads/GNU-Linux \
on how to setup M2 on your desired distro. Alternatively define $M2Path.";
InterfaceM2::munixm2 = "Missing Macaulay2 binary. \
Please check http://www2.macaulay2.com/Macaulay2/Downloads/GNU-Linux \
on how to setup M2 on your desired distro. Alternatively, define $M2Path.";
InterfaceM2::m2fld = "Missing Macaulay2 binary. \
Please check http://www2.macaulay2.com/Macaulay2/Downloads/GNU-Linux \
on how to setup M2 on your desired distro. Alternatively, define $M2Path.";

loadInterfaceM2[] := 
  Module[{cmdCheck, windowsCheck, unixCheck, fixPath},
    cmdCheck[cmd : (_String | {__String}), 
        tests : (_List | _Association | _Rule) : {}] :=
      Module[{proc},
        proc = Check[Quiet@RunProcess[cmd], $Failed];
        If[FailureQ@proc, Return@False];
        And @@ Join[
          {proc["ExitCode"] == 0},
          KeyValueMap[(#2@Lookup[#1]@proc) &]@
            KeyTake[Association@tests, Keys@proc]
        ]
      ];
    windowsCheck[] := Module[{whereM2, whereWSL, whereWSLM2},
      whereM2 = cmdCheck[{"where", "M2"},
        "StandardOutput" -> StringContainsQ["M2"]
      ];
      If[whereM2, $m2path = {"M2"}; Return[True] ];
      whereWSL = cmdCheck[{"where", "wsl"},
        "StandardOutput" -> StringContainsQ["wsl"]
      ];
      If[ !whereWSL && !whereM2, 
        Message[InterfaceM2::mdosm2]; Return[False] 
      ];
      If[ whereWSL, 
        whereWSLM2 = cmdCheck[{"wsl", "which", "M2"}, 
          "StandardOutput" -> StringContainsQ["M2"] ];
        If[whereWSLM2, 
          $m2path = {"wsl", "M2"}; Return[True],
          Message[InterfaceM2::mwslm2]; Return[False]
        ];
      ];
      Return[False];
    ];
    fixPath[] := Module[{},
      SetEnvironment["PATH" -> StringRiffle[Join[{
            If[ValuesQ[$M2Path] && MatchQ[$M2Path, _?DirectoryQ|{__?DirectoryQ}],
              Splice@Flatten@List[$M2Path], Nothing
            ],
            "/opt/homebrew/bin",
            "/opt/homebrew/sbin",
            "/usr/local/bin",
            "/usr/local/opt/macaulay2/bin"},
          StringSplit[Environment["PATH"], ":"]
        ], ":"]
      ];
    ];
    unixCheck[] := Module[{whichM2},
      whichM2 = cmdCheck[{"which", "M2"},
        "StandardOutput" -> 
          Not@*StringContainsQ["not found"]
      ];
      If[whichM2, 
        $m2path = {"M2"}; Return[True], 
        Message[InterfaceM2::munixm2]; Return[False]
      ];
      (*helpM2 = cmdCheck[{"M2", "--help"},
        "StandardOutput" -> StringContainsQ["M2"]
      ];*)
      Return[False];
    ];
    If[
      Switch[$OperatingSystem,
        "Windows", windowsCheck[],
        "MacOSX", fixPath[]; unixCheck[],
        _, unixCheck[]
      ],
      InterfaceM2`Core`KillM2[];
      Get["InterfaceM2`Core`"];
      Get["InterfaceM2`Ideals`"];
      InterfaceM2`Core`InitializeM2[$m2path];
    ];
  ];

loadInterfaceM2[];

ClearAll[loadInterfaceM2, $m2path];



