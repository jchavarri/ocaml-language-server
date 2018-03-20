import * as fs from "fs";
import * as path from "path";
import * as bucklescript from "../src/bin/server/parser/bucklescript";

const mocksFolder = "bucklescript.mocks";
const getDiagnostics = mockFileName => {
  const bsbOutput = fs.readFileSync(path.join(__dirname, mocksFolder, mockFileName), "utf8");
  return bucklescript.parseErrors(bsbOutput);
};

it('parses messages including "can\'t be found" strings', () => {
  expect(getDiagnostics("1.txt")).toMatchSnapshot();
});

it('parses messages including "Error while running external preprocessor"', () => {
  expect(getDiagnostics("2.txt")).toMatchSnapshot();
});

it('parses messages including "Error: 1087: Expecting one of the following"', () => {
  expect(getDiagnostics("3.txt")).toMatchSnapshot();
});

it("parses messages including multiple errors", () => {
  expect(getDiagnostics("4.txt")).toMatchSnapshot();
});

it('parses messages including "Unused bucklescript attribute"', () => {
  expect(getDiagnostics("5.txt")).toMatchSnapshot();
});

it("parses messages including interface/implementation mismatch", () => {
  expect(getDiagnostics("6.txt")).toMatchSnapshot();
});

it("parses messages including FFI errors with [@bs.send]", () => {
  expect(getDiagnostics("7.txt")).toMatchSnapshot();
});
