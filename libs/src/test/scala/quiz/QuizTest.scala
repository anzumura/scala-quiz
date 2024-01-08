package quiz

import quiz.Quiz.ListOrder
import quiz.data.KanjiDataSanityTest.data
import quiz.kanji.{Grade, Kyu, Level}
import quiz.utils.BaseChoiceTest

class QuizTest extends BaseChoiceTest:
  private val firstQuestion = "Question 1 of 250 (score 0):"

  private def quiz(input: String, random: Boolean = false) =
    val (choice, os) = create(input)
    Quiz(data, choice, random).start()
    os.toString
  private def questionPrompt(showMeanings: Boolean = true) =
    s"Choose reading (-=${if showMeanings then "show" else "hide"} meanings, 1-4, q=quit): "
  private val quizTypePrompt = "Quiz Type (f=Frequency, g=Grade, k=Kyu, l=Level, q=quit) def 'f': "

  "create quiz with default parameters" in { assert(Quiz().getClass.getSimpleName == "Quiz") }

  "prompt for choosing quiz type" in { assert(quiz("q") == quizTypePrompt) }

  "prompt to choose quiz type is shown again once a quiz is completed" in {
    val kanjiInGradeOne = data.gradeMap(Grade.G1).size
    // choose answer '1' for each question to complete the quiz
    val out = quiz(s"g\n1\nb\n${"1\n".repeat(kanjiInGradeOne)}q")
    assert(out.startsWith(quizTypePrompt))
    assert(out.matches(s"(?s).*>>> Final score: [0-9]*/$kanjiInGradeOne.*"))
    assert(out.endsWith(quizTypePrompt))
  }

  "prompt for Frequency quiz" in {
    assert(
      quiz("\nq").endsWith("Frequency buckets of 250 (0=most frequent, 1-9, q=quit) def '0': "))
  }

  "prompt for Grade quiz" in {
    assert(quiz("g\nq").endsWith("Grade (1-6, q=quit, s=Secondary School) def 's': "))
  }

  "prompt for Level quiz" in { assert(quiz("l\nq").endsWith("JLPT Level (1-5, q=quit) def '1': ")) }

  "prompt for Kyu quiz" in {
    assert(quiz("k\nq").endsWith("Kentei Kyu (0=k10, 1-9, j=Jun-K2, k=Jun-K1, q=quit) def '0': "))
  }

  "prompt for list order" in {
    assert(quiz("\n\nq")
        .endsWith("List order (b=from beginning, e=from end, q=quit, r=random) def 'b': "))
  }

  private val beginQuestion = s"$firstQuestion 日. Rad 日(72), Strokes 4, G1, N5, K10"
  private val endQuestion = s"$firstQuestion 価. Rad 人(9), Strokes 8, G5, N2, K6"
  // random seed is constant for test code so `randomQuestion` value shouldn't change
  private val randomQuestion = s"$firstQuestion 安. Rad 宀(40), Strokes 6, G3, N5, K8"

  "list order is changed based on choice" in
    ListOrder.values.zip(Array(beginQuestion, endQuestion, randomQuestion))
      .foreach((x, msg) => assert(quiz(s"\n\n${x.value}\nq").contains(msg), s" -- testing $x"))

  "list order is random when random is seed not constant" in {
    val quizInput = "\n\nr\nq"
    // run with questions in random order, but constant seed should produce expected result
    assert(quiz(quizInput).contains(randomQuestion))
    // run large number of times with random seed should produce at least some different results
    val out = Array.fill(50)(quiz(quizInput, random = true))
    assert(out.exists(!_.contains(randomQuestion)))
  }

  "question contains 4 choices followed by a prompt" in {
    assert(quiz("\n\n\nq").contains(s"""
        |  1: ガ、カク
        |  2: ニチ、ジツ、ひ、か
        |  3: ム、つと-める、つと-まる
        |  4: レン、つら-なる、つら-ねる、つ-れる
        |${questionPrompt()}""".stripMargin))
  }

  "Frequency buckets 0-8 have 250 entries and bucket 9 has 251" in (0 to 9).foreach(x =>
    assert(quiz(s"\n$x\nb\nq").contains("Question 1 of 25" + (if x == 9 then 1 else 0))))

  "Grades have expected sizes" in Grade.defined.foreach(x =>
    assert(quiz(s"g\n${if x == Grade.S then 's' else x.toString.last}\nb\nq")
        .contains("Question 1 of " + data.gradeMap(x).size)))

  "Levels have expected sizes" in Level.defined.foreach(x =>
    assert(quiz(s"l\n${x.toString.last}\nb\nq").contains("Question 1 of " + data.levelMap(x).size)))

  "Kyus have expected sizes" in Kyu.defined.foreach(x =>
    assert(quiz(s"k\n${x match
        case Kyu.K10 => '0'
        case Kyu.KJ2 => 'j'
        case Kyu.KJ1 => 'k'
        case _ => x.toString.last
      }\nb\nq").contains("Question 1 of " + data.kyuMap(x).size)))

  "correct answer increases score" in {
    val correct = 2 // can rely on this since random seed is constant for test code
    (1 to 4).foreach(x =>
      assert(quiz(s"\n\n\n$x\nq")
          .contains(s"Question 2 of 250 (score ${if x == correct then 1 else 0})")))
  }

  "incorrect answer results in a message to the user containing the correct answer" in {
    assert(quiz("\n\n\n1\nq").contains("  Incorrect: the answer is 2"))
  }

  "final score should be 0/0 if user quits before answering the first question" in {
    assert(quiz("\n\n\nq").contains(">>> Final score: 0/0\n"))
  }

  "final score should be 1/1 after answering one question correctly and then quitting" in {
    assert(quiz("\n\n\n2\nq").contains(">>> Final score: 1/1\n"))
  }

  "final score should be 0/1 and show mistakes after answering a question incorrectly" in {
    assert(quiz("\n\n\n1\nq").contains(">>> Final score: 0/1, mistakes: 日\n"))
  }

  "show and hide meanings" in {
    val result = quiz("\n\n\n-\n-\nq")
    countString(result, beginQuestion, 3)            // expect question to be shown 3 times
    countString(result, beginQuestion + " - day", 1) // expect 'question with meaning' 1 time
    countString(result, questionPrompt(), 2)         // expect 'show meaning' prompt 2 times
    countString(result, questionPrompt(false), 1)    // expect 'hide meaning' prompt 1 time
  }
