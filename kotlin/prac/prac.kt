package my.demo

fun sum(x: Int, y: Int): Int {
    return x + y
}

fun noret() {
    println("noreturn")
}

fun useTmpl(str: String) {
    println("Use template ${str}")
}

fun sumvar(x: Int, y:Int): Int {
    var total = x + y
    return total
}

val PI = 3.14

fun sumgvar(x: Int): Double {
    return x * x * PI
}

// one line comment

/*
 * multiple line comments
 */

fun greater(x: Int, y: Int): Int {
    if (x > y) {
        return x
    } else {
        return y
    }
}

fun anotherGreater(a: Int, b: Int) = if (a > b) a else b

fun parseInt(str: String): Int? {
    return str.toIntOrNull()
}

fun nullcheck(strX: String, strY: String) {
    val x = parseInt(strX)
    val y = parseInt(strY)

    if (x != null && y != null) {
        println(x * y)
    } else {
        println("'$strX' or '$strY' is not a number")
    }
}

fun typeCheck(obj: Any): Int? {
    if (obj is String) {
        return obj.length
    }
    return null
}

val items = listOf("apple", "banana", "cucumber")

fun forloop() {
    for (item in items) {
        print(item)
        print(" ")
    }
    println()
}

fun anotherforloop() {
    for (index in items.indices) {
        println("item at $index is ${items[index]}")
    }
}

fun whileloop() {
    var index = 0
    while (index < items.size) {
        println("item at $index is ${items[index]}")
        index++
    }
}

fun whensample(obj: Any): String =
    when (obj) {
        1          -> "One"
        "Hello"    -> "Greeting"
        is Long    -> "Long"
        !is String -> "Not a string"
        else       -> "Unknown"
    }

open class Person {
  val firstName: String
  val lastName: String
  var age: Int

  constructor(firstName: String, lastName: String, age: Int) {
    this.firstName = firstName
    this.lastName = lastName
    this.age = age
  }

  open fun getFullName(): String {
    return firstName + " " + lastName
  }

  open val reverseName: String
    get() {
      return lastName + " " + firstName
    }
}

class EasternPerson(firstName: String, lastName: String, age: Int) : Person(firstName, lastName, age) {
  override val reverseName: String
    get() = firstName + " " + lastName
}

fun main(args: Array<String>) {
    println("--- Hello, world! ---")

    println("--- function sample ---")
    print("2 plus 3 is: ")
    println(sum(2,3))

    println("--- no return function ---")
    noret()

    println("--- template function ---")
    useTmpl("input")

    println("--- variable in function ---")
    println(sumvar(1,3))

    println("--- global variable (calculate circle area) ---")
    println(sumgvar(4))

    println("--- conditional ---")
    println(greater(5, 7))

    println("--- another conditional func ---")
    println(anotherGreater(4, 1))

    println("--- if as expression ---")
    val foo = 30
    val bar = if (foo > 20) "greater than 20" else "less than 20"
    println(bar)

    println("--- null check sample1 ---")
    nullcheck("1", "2")
    println("--- null check sample2 ---")
    nullcheck("str", "2")

    println("--- type check sample1 ---")
    println(typeCheck("typecheck"))
    println("--- type check sample2 ---")
    println(typeCheck(2))

    println("--- for loop sample1 ---")
    forloop()
    println("--- for loop sample2 ---")
    anotherforloop()

    println("--- while loop sample ---")
    whileloop()

    println("--- when sample1 ---")
    println(whensample("Hello"))
    println("--- when sample2 ---")
    println(whensample(1))

    println("--- range sample1 ---")
    val a1 = 10
    val a2 = 9
    if (a1 in 1..a2+1) {
        println("fits in range")
    }

    println("--- range sample2 ---")
    for (x in 1..5) {
        print(x)
    }
    println()

    println("--- range sample3 ---")
    for (x in 1..10 step 2) {
        print(x)
    }
    println()
    println("--- range sample4 ---")
    for (x in 9 downTo 0 step 3) {
        print(x)
    }
    println()

    println("--- collection sample1 ---")
    val items = setOf("apple", "banana", "kiwifruit")
    when {
        "orange" in items -> println("juicy")
        "apple" in items -> println("apple is fine too")
    }

    println("--- collection sample2 ---")
    val fruits = listOf("banana", "avocado", "apple", "kiwifruit")
    fruits
        .filter { it.startsWith("a") }
        .sortedBy { it }
        .map { it.toUpperCase() }
        .forEach { println(it) }

    println("--- class sample ---")
    val p = Person("matsu", "yoshi", 20)
    println(p.getFullName())
    println(p.reverseName)

    println("--- override sample ---")
    val ep = EasternPerson("foo", "bar", 30)
    println(ep.reverseName)
}
