package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = {
    {w.toLowerCase groupBy (x => x) map {case (key, value) => (key, value.length)}}.toList.sortBy(x => x._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy (x => wordOccurrences(x))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))


  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty) List(List())
    else {
      val head = occurrences.head
      val combos : List[Occurrences] = combinations(occurrences.tail)
      (for {
        combo <- combos
        x <- (1 to head._2)
      } yield (head._1, x) :: combo) ++ combos
    }
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    y.foldLeft(x.toMap)((occs, occ) =>
      if (occ._2 == occs(occ._1)) occs - occ._1
      else occs updated (occ._1, (occs(occ._1) - occ._2))
    ).toList
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def iter(occurrences : Occurrences) : List[Sentence] = {
      if (occurrences.isEmpty) List(Nil)
      else for {
        combination <- combinations(occurrences)
        word <- dictionaryByOccurrences getOrElse (combination, Nil)
        rest <- iter(subtract(occurrences, wordOccurrences(word)))
        if !combination.isEmpty
      } yield word :: rest
    }
    iter( sentenceOccurrences(sentence) )
  }
}
