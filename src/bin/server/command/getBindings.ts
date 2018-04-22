import * as LSP from "vscode-languageserver-protocol";
import { refmt as refmtParser } from "../parser";
import * as processes from "../processes";
import Session from "../session";

// Temporary measure until there is some persisted list of diagnostics shared between services
let lastDiagnostics: LSP.Diagnostic[] = [];
export default async function(
  session: Session,
  doc: LSP.TextDocumentIdentifier,
  javascriptCode: string,
): Promise<string> {
  const rebind = new processes.Rebind(session).process;
  const refmt = new processes.ReFMT(session, undefined, ["--parse", "binary", "--print", "re"]).process;
  rebind.stdout.pipe(refmt.stdin);
  rebind.stdin.write(javascriptCode);
  rebind.stdin.end();
  const otxt = await new Promise<string>((resolve, reject) => {
    let buffer = "";
    let bufferError = "";
    refmt.stdout.on("error", (error: Error) => reject(error));
    refmt.stdout.on("data", (data: Buffer | string) => (buffer += data.toString()));
    refmt.stdout.on("end", () => resolve(buffer));

    refmt.stderr.on("data", (data: Buffer | string) => (bufferError += data.toString()));
    refmt.stderr.on("end", () => {
      const diagnostics = refmtParser.parseErrors(bufferError);
      if (diagnostics.length !== 0 || diagnostics.length !== lastDiagnostics.length) {
        session.connection.sendDiagnostics({
          diagnostics,
          uri: doc.uri,
        });
      }
      lastDiagnostics = diagnostics;
    });
  });
  rebind.unref();
  refmt.unref();
  return /^\s*$/.test(otxt) ? "" : otxt.trim();
}
