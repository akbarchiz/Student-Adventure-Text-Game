package o1.adventure

/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game. */
class Adventure:

  /** the name of the game */
  val title = "A Student Adventure"

  private val spawnHome       = Area("Otakaari 18", "Otakaari 18, this is where you live. You see your neighbor here, you may want to talk to him",
    NonPlayers(this, "Shishir", "Shishir: Hi my name is Shishir, I'm a mechanical engineering student. " +
      "\nAs you know we are planning to launch a rocket. Your task is to prepare fuel for our rocket." +
      "\nYou can buy chemicals in Labofarma, if you need money you should go for Ivan. He's at math exercise session right now" +
      "\nBefore you go, I'm giving this rocket model to you just in case Oliver forgot.",  "I'm doing my physics homework, go away!",
       "rocket model", "nothing"), this)

  private val lab             = Area("Lab", "Lab, this is where you cook. Talk to professor",
    NonPlayers(this, "Mike", "Mike: Hi I'm professor Mike. Before taking lab take these gloves.","Mike: Please come back later",
       "gloves", "nothing"), this)

  private val laboFarma       = Area("Labofarma", "Labofarma, this is where you buy chemicals. Talk to seller.",
    NonPlayers(this, "Jesse", "Jesse: As you're my first customer here, take this candy as a gift. Anyways, buy chemicals and make magic","Jesse: You wanna buy or not?",
       "candy", "chemicals"),this)

  private val lake            = Area("Lake", "Lake, there is someone swimming...",
    NonPlayers(this, "Swimming guy", "Swimming guy: *Bulb*", "Swimming guy: Bulb",
       "pants", "nothing"),this)

  private val launchArea      = Area("Launch Area", "Launch Area, this is where you launch your rocket. Talk to ME students",
    NonPlayers(this, "Oliver", "Oliver: Hey Walter we have everything. Where is your fuel?","Oliver: I don't want to wait, time is money!",
       "rocket stuff", "nothing"),this)

  private val basketballArea  = Area("Basketball area", "Basketball area, someone is playing in this area. Talk to npc",
    NonPlayers(this, "Bryan", "Bryan: Wassup man, I'm Bryan. Here is the ball. Show me what u got","Bryan: What do u want?.",
       "ball", "money"),this)

  private val university      = Area("University", "This is University. Talk with npc",
    NonPlayers(this, "Ivan", "Ivan: I was informed u need money... \n\nThere is rich student named Bryan playing basketball. Here is the gun. If you find him, you know what to do...","Ivan: I don't want to talk right now",
        "weapon", "nothing"),this)

  private val destination     = launchArea


  spawnHome.setNeighbors(Vector("north" -> lab, "east" -> launchArea, "south" -> laboFarma, "west" -> lake))
  lab.setNeighbors(Vector("east" -> launchArea, "south" -> spawnHome))
  laboFarma.setNeighbors(Vector("north" -> spawnHome, "east" -> university, "west" -> basketballArea))
  lake.setNeighbors(Vector("north" -> lab, "east" -> spawnHome, "south" -> basketballArea))
  launchArea.setNeighbors(Vector("west" -> spawnHome, "south" -> university))
  basketballArea.setNeighbors(Vector("north" -> lake, "east" -> laboFarma))
  university.setNeighbors(Vector("north" -> launchArea, "west" -> laboFarma))



  /** The character that the player controls in the game. */
  val player = Player(spawnHome)

  
  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 80


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.destination && this.player.has("Solid fuel")

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || this.player.arrested

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "Walter, a first year chemical engineering student, has now holidays." +
    "\n\nHis neighbors: Shishir and Oliver are mechanical engineering bachelor students, and planned to launch a rocket this holiday. " +
    "They have already built a rocket, the only thing they need is fuel."+
    "\n\nWalter decides to use KNO3 and sugar to make solid fuel for their rocket. He as a student lacks money and needs to earn them first to buy chemicals."


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage =
    if this.isComplete then
      "Successful launch! Walter finally achieved his dream, his rocket achieved 2km altitude. \nCongratulations! YOU WON!"
    else if this.turnCount == this.timeLimit then
      "Oh no! Time's up. Starved of entertainment, you collapse and weep like a child.\nGame over!"
    else if this.player.arrested then
      "YOU ARE ARRESTED. GAME OVER"
    else  // game over due to player quitting
      "Quitter!"


  /** Plays a turn by executing the given in-game command, such as “go west”. Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) =
    val action = Action(command)
    val outcomeReport = action.execute(this.player)
    if outcomeReport.isDefined then
      this.turnCount += 1
    outcomeReport.getOrElse(s"Unknown command: \"$command\".")

end Adventure 