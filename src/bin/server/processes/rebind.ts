import { ChildProcess } from "child_process";
import Session from "../session";

export default class Rebind {
  public readonly process: ChildProcess;
  constructor(session: Session, argsOpt?: string[]) {
    const command = session.settings.reason.path.rebind;
    const args = argsOpt;
    this.process = session.environment.spawn(command, args);
  }
}
