external vscodeLangServerTypes : Js.t {..} = "vscode-languageserver-types" [@@bs.module];

/* TODO: add to glennsl/bs-vscode */
module DiagnosticSeverity = {
  type t;
  let error: t = vscodeLangServerTypes##_DiagnosticSeverity##_Error;
  let warning: t = vscodeLangServerTypes##_DiagnosticSeverity##_Warning;
  let information: t = vscodeLangServerTypes##_DiagnosticSeverity##_Information;
  let hint: t = vscodeLangServerTypes##_DiagnosticSeverity##_Hint;
};

type source =
  | Bucklescript
  | Merlin;

let sourceToString source =>
  switch source {
  | Bucklescript => "bucklescript"
  | Merlin => "merlin"
  };

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
  severity: DiagnosticSeverity.t,
  source: string
};

let getSeverity input => {
  let severityRe = [%re "/^Warning number \\d+/"];
  switch (severityRe |> Js.Re.exec input) {
  | Some _ => DiagnosticSeverity.warning
  | None => DiagnosticSeverity.error
  }
};

let createDiagnostic message startCharacter startLine endCharacter endLine severity => {
  "code": "",
  "message": message,
  "range": {
    "end": {character: endCharacter, line: endLine},
    "start": {character: startCharacter, line: startLine}
  },
  "severity": severity,
  "source": sourceToString Bucklescript
};

let unsafeGetMatch match =>
  switch (Js.Nullable.to_opt match) {
  | Some m => m
  | None => raise (Failure "Match not found")
  };

let parseErrors bsbOutput => {
  let parsedDiagnostics = Js.Dict.empty ();
  let level1 = [
    [%re "/File \"(.*)\", line (\\d*), characters (\\d*)-(\\d*):[\\s\\S]*?/"],
    [%re "/Error: ([\\s\\S]*)We've found a bug for you!/"]
  ];
  let reLevel1Errors =
    Js.Re.fromStringWithFlags (String.concat "" (List.map Js.Re.source level1)) flags::"g";
  let break = ref false;
  while (not !break) {
    switch (reLevel1Errors |> Js.Re.exec bsbOutput) {
    | None => break := true
    | Some result =>
      let matches = Js.Re.matches result;
      let fileUri = "file://" ^ unsafeGetMatch matches.(1);
      let startLine = int_of_string (unsafeGetMatch matches.(2)) - 1;
      let endLine = startLine;
      let startCharacter = int_of_string (unsafeGetMatch matches.(3));
      let endCharacter = int_of_string (unsafeGetMatch matches.(4));
      let message = String.trim (unsafeGetMatch matches.(5));
      let severity = getSeverity (unsafeGetMatch matches.(0));
      let diagnostic =
        createDiagnostic message startCharacter startLine endCharacter endLine severity;
      let diagnostics =
        switch (Js.Dict.get parsedDiagnostics fileUri) {
        | Some d => Js.Array.concat [|diagnostic|] d
        | None => [|diagnostic|]
        };
      Js.Dict.set parsedDiagnostics fileUri diagnostics
    }
  };
  let level2 = [
    [%re
      "/(?:We've found a bug for you!|Warning number \\d+)\\n\\s*/"
    ], /* Heading of the error / warning */
    [%re
      "/(.*) (\\d+):(\\d+)(?:-(\\d+)(?::(\\d+))?)?\\n  \\n/"
    ], /* Capturing file name and lines / indexes */
    [%re "/(?:.|\\n)*?\\n  \\n/"], /* Ignoring actual lines content being printed */
    [%re "/((?:.|\\n)*?)/"], /* Capturing error / warning message */
    [%re
      "/((?=We've found a bug for you!)|(?:\\[\\d+\\/\\d+\\] (?:\\x1b\\[[0-9;]*?m)?Building)|(?:ninja: build stopped: subcommand failed)|(?=Warning number \\d+)|$)/"
    ] /* Possible tails */
  ];
  let reLevel2Errors =
    Js.Re.fromStringWithFlags (String.concat "" (List.map Js.Re.source level2)) flags::"g";
  let break = ref false;
  while (not !break) {
    switch (reLevel2Errors |> Js.Re.exec bsbOutput) {
    | None => break := true
    | Some result =>
      let matches = Js.Re.matches result;
      let fileUri = "file://" ^ unsafeGetMatch matches.(1);
      let startLine = int_of_string (unsafeGetMatch matches.(2)) - 1;
      let startCharacter = int_of_string (unsafeGetMatch matches.(3)) - 1;
      let (endLine, endCharacter) =
        switch (Js.Nullable.to_opt matches.(4), Js.Nullable.to_opt matches.(5)) {
        /* Format is SL-SC:EL-EC */
        | (Some fourth, Some fifth) => (int_of_string fourth - 1, int_of_string fifth)
        /* Format is L:SC-EC */
        | (Some fourth, None) => (startLine, int_of_string fourth)
        /* Format is L:C */
        | (None, None) => (startLine, startCharacter)
        /* Impossible */
        | (None, Some _) => raise (Failure "Impossible state")
        };
      let messageRe = [%re "/\\n  /g"];
      let message = Js.String.replaceByRe messageRe "\\n" (unsafeGetMatch matches.(6));
      let severity = getSeverity (unsafeGetMatch matches.(0));
      let diagnostic =
        createDiagnostic message startCharacter startLine endCharacter endLine severity;
      let diagnostics =
        switch (Js.Dict.get parsedDiagnostics fileUri) {
        | Some d => Js.Array.concat [|diagnostic|] d
        | None => [|diagnostic|]
        };
      Js.Dict.set parsedDiagnostics fileUri diagnostics
    }
  };
  Js.log parsedDiagnostics;
  parsedDiagnostics
};
