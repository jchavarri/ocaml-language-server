import * as LSP from "vscode-languageserver-protocol";
import { types } from "../../../lib";
import * as command from "../command";
import Session from "../session";

export default function(session: Session): LSP.RequestHandler<types.IGetBindingsParams, string, void> {
  return event => command.getBindings(session, event.doc, event.javascriptCode);
}
