type source =
  | Bucklescript
  | Merlin;

type lineAndChar = {
  character: int,
  line: int
};

type range = {
  endPos: lineAndChar,
  startPos: lineAndChar
};

type diagnostic = {
  code: string,
  message: string,
  range,
  severity: int,
  source
};

let createDiagnostic message startCharacter startLine endCharacter endLine severity => {
  code: "",
  message,
  range: {
    endPos: {character: endCharacter, line: endLine},
    startPos: {character: startCharacter, line: startLine}
  },
  severity,
  source: Bucklescript
};

let parseErrors bsbOutput => {
  let parsedDiagnostics = {};
  let level1 = [
    [%re "/File \"(.*)\", line (\\d*), characters (\\d*)-(\\d*):[\\s\\S]*?/"],
    [%re "/Error: ([\\s\\S]*)We've found a bug for you!/"]
  ];
  let reLevel1Errors = String.concat "" (List.map Js.Re.source level1);
  let result = Js.Re.exec bsbOutput (Js.Re.fromString reLevel1Errors);
  let errorMatch = ref false;
  while (errorMatch = reLevel1Errors.exec bsbOutput)
    {
      let fileUri = "file://" + errorMatch [1];
      let startLine = Number (errorMatch [2]) - 1;
      let endLine = Number (errorMatch [2]) - 1;
      let startCharacter = Number (errorMatch [3]);
      let endCharacter = Number (errorMatch [4]);
      let message = errorMatch [5].trim ();
      let severityRe = [%re "/^Warning number \\d+/"];
      let result = Js.Re.exec errorMatch [0] severityRe;
      let severity =
        switch result {
        | Some result => Some result
        | None => None /* types.DiagnosticSeverity.Error */
        };
      let diagnostic =
        createDiagnostic (message, startCharacter, startLine, endCharacter, endLine, severity);
      ()
    };
    /* if (!parsedDiagnostics[fileUri]) { parsedDiagnostics[fileUri] = []; }
       parsedDiagnostics[fileUri].push(diagnostic); */
  let level2 = [
    [%re
      "/(?:We've found a bug for you!|Warning number \\d+)\\n\\s*/"
    ], /* Heading of the error / warning */
    [%re
      "/(.*) (\\d+):(\\d+)(?:-(\\d+)(?::(\\d+))?)?\\n  \\n/"
    ], /* Capturing file name and lines / indexes */
    [%re "/(?:.|\\n)*?\\n  \\n/"], /* Ignoring actual lines content being printed */
    [%re "/((?:.|\n)*?)/"], /* Capturing error / warning message */
    [%re
      "/((?=We've found a bug for you!)|(?:\\[\\d+\\/\\d+\\] (?:\\x1b\\[[0-9;]*?m)?Building)|(?:ninja: build stopped: subcommand failed)|(?=Warning number \\d+)|$)/"
    ] /* Possible tails */
  ];
  let reLevel2Errors = String.concat "" (List.map Js.Re.source level2);
  let result = Js.Re.exec bsbOutput (Js.Re.fromString reLevel2Errors);
  let errorMatch = ref false;
  while (errorMatch = reLevel2Errors.exec bsbOutput)
    {
      let fileUri = "file://" + errorMatch [1];
      /* Suppose most complex case, path/to/file.re 10:20-15:5 message */
      let startLine = Number (errorMatch [2]) - 1;
      let startCharacter = Number (errorMatch [3]) - 1;
      let endLine = Number (errorMatch [4]) - 1;
      let endCharacter = Number (errorMatch [5]); /* Non inclusive originally */
      let messageRe = [%re "/\\n  /g"];
      let message = errorMatch [6].replace (messageRe, "\n");
      if (isNaN endLine) {
        /* Format path/to/file.re 10:20 message */
        endCharacter = startCharacter;
        endLine = startLine
      } else if (
        isNaN endCharacter
      ) {
        /* Format path/to/file.re 10:20-15 message */
        endCharacter = endLine + 1; /* Format is L:SC-EC */
        endLine = startLine
      };
      let severityRe = [%re "/^Warning number \\d+/"];
      let result = Js.Re.exec errorMatch [0] severityRe;
      let severity =
        switch result {
        | Some result => Some result
        | None => None /* types.DiagnosticSeverity.Error */
        };
      let diagnostic =
        createDiagnostic (message, startCharacter, startLine, endCharacter, endLine, severity);
      ()
    }
    /* if (!parsedDiagnostics[fileUri]) { parsedDiagnostics[fileUri] = []; }
       parsedDiagnostics[fileUri].push(diagnostic); */
  /* return parsedDiagnostics; */
};
