import org.scalatest.FunSuite

class HelloWorldTest extends  FunSuite{
  test("HelloWorld.cube") {
    assert(HelloWorld.cube(3) === 27)
  }
  test("HelloWorld.sayHello"){
    assert(HelloWorld.sayHello === "Hello, World!")
  }
}
