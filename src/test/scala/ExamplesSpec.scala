package mk262968.javalette;
package test;

import org.scalatest.Spec

import java.io.File;

class ExamplesSpec extends Spec {
  describe("Javalette Compiler") {
    val examplesDir = new File("examples");
    val goodDir = new File(examplesDir, "good");
    val badDir = new File(examplesDir, "bad");
    goodDir.listFiles foreach { file =>
      if(file.getName.endsWith(".jl"))
        it("should compile %s".format(file.getName)) {
          Main.compile(file);
        }
    }
    badDir.listFiles foreach { file =>
      if(file.getName.endsWith(".jl"))
        it("should not compile %s".format(file.getName)) {
          expect(true) {try {
            Main.compile(file);
            false
          }
          catch {
            case e: ast.LexerError => true
            case e: ast.ParserError => true
            case e: CompilationError => true
          }
        }}
    }
  }
}

