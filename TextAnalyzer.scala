import scala.io.Source
import scala.collection.mutable
import scala.util.{Try, Success, Failure}

object TextAnalyzer extends App {

  if (args.length != 1) {
    println("Для работы программы введите: TextAnalyzer <путь к файлу>")
    sys.exit(1)
  }

  val filePath = args(0)

  // Чтение содержимого файла с обработкой ошибок
  val text = Try(Source.fromFile(filePath).getLines().mkString(" ")) match {
    case Success(content) => content
    case Failure(exception) =>
      println(s"Ошибка чтения файла: ${exception.getMessage}")
      sys.exit(1)
  }

  if (text.isEmpty) {
    println("Файл пуст (Казна пуста, Милорд).")
    sys.exit(1)
  }

  // Функция для подсчета количества слов
  def wordCount(text: String): Int = text.split("\\s+").length

  // Функция для частотного анализа слов
  def wordFrequency(text: String): Map[String, Int] = {
    val words = text.toLowerCase.split("\\s+")
    val freqMap = mutable.Map[String, Int]().withDefaultValue(0)
    for (word <- words) {
      freqMap(word) += 1
    }
    freqMap.toMap
  }

  // Функция для нахождения наиболее часто встречающихся слов
  def mostCommonWords(freqMap: Map[String, Int], topN: Int): List[(String, Int)] = {
    freqMap.toList.sortBy(-_._2).take(topN)
  }

  // Функция для анализа длины предложений
  def sentenceLengthAnalysis(text: String): (Double, Int, Int) = {
    val sentences = text.split("[.!?]+").map(_.trim).filter(_.nonEmpty)
    val lengths = sentences.map(_.split("\\s+").length)
    val avgLength = if (lengths.nonEmpty) lengths.sum.toDouble / lengths.length else 0
    val minLength = if (lengths.nonEmpty) lengths.min else 0
    val maxLength = if (lengths.nonEmpty) lengths.max else 0
    (avgLength, minLength, maxLength)
  }

  // Подсчет количества слов
  val totalWords = wordCount(text)

  // Частотный анализ слов
  val frequencies = wordFrequency(text)

  // Нахождение наиболее часто встречающихся слов
  val topN = 10
  val commonWords = mostCommonWords(frequencies, topN)

  // Анализ длины предложений
  val (avgLength, minLength, maxLength) = sentenceLengthAnalysis(text)

  // Функция для создания строки таблицы
  def formatRow(cells: Seq[String], cellWidths: Seq[Int]): String = {
    cells.zip(cellWidths).map { case (cell, width) =>
      cell.padTo(width, ' ')
    }.mkString("| ", " | ", " |")
  }

  // Создание таблицы для общей информации
  val headers = Seq("Метрика", "Значение")
  val rows = Seq(
    Seq("Общее количество слов", totalWords.toString),
    Seq("Средняя длина предложения", f"$avgLength%.2f слов"),
    Seq("Минимальная длина предложения", minLength.toString),
    Seq("Максимальная длина предложения", maxLength.toString)
  )
  val colWidths = headers.map(_.length).zipAll(rows.transpose.map(_.map(_.length).max), 0, 0).map { case (h, r) => Math.max(h, r) }
  
  val table = (Seq(headers) ++ rows).map(formatRow(_, colWidths))
  val separator = colWidths.map("-" * _).mkString("+-", "-+-", "-+")

  // Создание таблицы для частотного анализа
  val freqHeaders = Seq("Слово", "Количество")
  val freqRows = commonWords.map { case (word, count) => Seq(word, count.toString) }
  val freqColWidths = freqHeaders.map(_.length).zipAll(freqRows.transpose.map(_.map(_.length).max), 0, 0).map { case (h, r) => Math.max(h, r) }
  
  val freqTable = (Seq(freqHeaders) ++ freqRows).map(formatRow(_, freqColWidths))
  val freqSeparator = freqColWidths.map("-" * _).mkString("+-", "-+-", "-+")

  // Печать таблиц
  println(separator)
  table.foreach(row => {
    println(row)
    println(separator)
  })

  println("Топ " + topN + " самых распространенных слов:")
  println(freqSeparator)
  freqTable.foreach(row => {
    println(row)
    println(freqSeparator)
  })
}
