import scala.io.StdIn.readLine

def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
        return (false, Some("Name cannot be empty."))
    } else if (marks > totalMarks) {
        return (false,
        Some(
            "Marks should not exceed the total possible marks."
        ))
    } else {
        return (true, None)
    }
}

def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student's name:")
    val name = readLine()
    println("Enter student's marks:")
    var marks = readLine().toInt
    println("Enter total possible marks:")
    val totalMarks = readLine().toInt

    if (marks < 0) marks = 0

    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
        case p if p >= 90 => 'A'
        case p if p >= 75 => 'B'
        case p if p >= 50 => 'C'
        case _            => 'D'
    }

    return (name, marks, totalMarks, percentage, grade)
}

def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var valid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

    while (!valid) {
        val (name, marks, totalMarks, percentage, grade) = getStudentInfo()
        val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
        if (isValid) {
            valid = true
            studentInfo = (name, marks, totalMarks, percentage, grade)
        } 
        else {
            println(s"Invalid input: ${errorMessage.get}")
        }
    }

    return studentInfo
}

def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks: $marks / $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
}

@main def main2(): Unit = {
    val studentRecord = getStudentInfoWithRetry()
    printStudentRecord(studentRecord)
}