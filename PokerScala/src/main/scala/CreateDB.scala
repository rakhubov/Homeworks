import java.util.UUID

object DbCommon {

  val authorOdersky: UUID = UUID.randomUUID()
  val authorRowling: UUID = UUID.randomUUID()
  val bookScala: UUID = UUID.randomUUID()
  val bookHPStone: UUID = UUID.randomUUID()
  val bookHPSecrets: UUID = UUID.randomUUID()

  val createTablePlayerRegistrationSql: String =
    """CREATE TABLE registration (
      |  id UUID PRIMARY KEY,
      |  name VARCHAR(100) NOT NULL,
      |  moneyPersonalAccount INT);""".stripMargin

  val createTableGameTableSql: String =
    """CREATE TABLE table (
      |  id UUID PRIMARY KEY,
      |  startGame BIT,
      |  namePlayer TEXT,
      |  bidForTable INT,
      |  dealerName UUID,
      |  playerInGame TEXT,
      |  numberOpenCard INT,
      |  generatedCards VARCHAR(200));""".stripMargin

  val createTablePlayerAtTableSql: String =
    """CREATE TABLE player (
      |  playerID UUID PRIMARY KEY,
      |  tableID UUID NOT NULL,
      |  name VARCHAR(100) NOT NULL,
      |  money INT,
      |  playerBid INT,
      |  playerCard VARCHAR(20),
      |  tableAndPlayerCard VARCHAR(70),
      |  cardForCombination VARCHAR(50),
      |  FOREIGN KEY (table) REFERENCES authors(id));""".stripMargin

  val populateDataSql: String =
    s"""
       |INSERT INTO authors (id, name, birthday) VALUES
       |  ('$authorOdersky', 'Martin Odersky', '1958-09-05'),
       |  ('$authorRowling', 'J.K. Rowling', '1965-07-31');
       |
       |INSERT INTO books (id, author, title, genre, year) VALUES
       |  ('$bookScala', '$authorOdersky', 'Programming in Scala', 'science', 2016),
       |  ('$bookHPStone', '$authorRowling', 'Harry Potter and Philosopher''s Stone', 'fantasy', 1997),
       |  ('$bookHPSecrets', '$authorRowling', 'Harry Potter and the Chamber of Secrets', 'fantasy', 1998);
       |""".stripMargin

}
