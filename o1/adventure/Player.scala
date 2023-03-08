package o1.adventure

import scala.collection.mutable.Map

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea  the player’s initial location */
class Player(startingArea: Area):
  private var killCounts = 0
  private var database = Map[String, Item]()
  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var itemNamesData = database.keys
  var arrested = false


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the player’s current location. */
  def location = this.currentLocation


  /** Drops the specified item in the player's current location.
   * Returns a description of the result: "You drop the ITEM_NAME." or "You don't have that!"
   */
  def drop(itemName: String): String =
    if has(itemName) then
      database.remove(itemName) match
        case Some(item) =>
          location.addItem(item)
          s"You drop the ${itemName}."
        case None =>
          "You don't have that!"
    else
      "You don't have that!"


  /** Examines the specified item in the player's inventory.
   * Returns a description of the result: "You look closely at the ITEM_NAME. DESCRIPTION" or "If you want to examine something, you need to pick it up first."
   */
  def examine(itemName: String): String =
    if has(itemName) then
      s"You look closely at the ${itemName}.\n${database(itemName).description}"
    else
       "If you want to examine something, you need to pick it up first."


  /** Picks up the specified item from the player's current location.
   * Returns a description of the result: "You pick up the ITEM_NAME." or "There is no ITEM_NAME here to pick up."
   */
  def get(itemName: String): String =
    if location.contains(itemName) then
      database(itemName) = location.removeItem(itemName).get
      s"You pick up the ${itemName}."
    else
      s"There is no ${itemName} here to pick up."


  /** Returns a description of the items in the player's inventory: "You are carrying:\nITEM_NAMES".
   * or "You are empty-handed."
   */
  def inventory: String =
    if database.nonEmpty then
      "You are carrying:\n" + database.keys.mkString("\n")
    else
      "You are empty-handed."


   /** Returns true if the player has the specified item in their inventory, false otherwise. */
  def has(itemName: String): Boolean =
    database.contains(itemName)


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player’s current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) =
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if destination.isDefined then "You go " + direction + "." else "You can't go " + direction + "."


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() =
    "You rest for a while. Better get a move on, though."


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() =
    this.quitCommandGiven = true
    ""


  /** Causes the player to talk to the non-player characters in the current area.
  * Returns a description of what the non-player characters said. */
  def talk() =
    this.currentLocation.speech()


  /** Returns a string that displays the player's current wanted level. The wanted level is determined by
  * the number of non-player characters that the player has killed. The more kills, the higher the wanted level.
  * The wanted level is represented by asterisks, for example: "Your wanted level: ***". */
  def wantedLevel() =
    "Your wanted level :" + "*" * this.killCounts


  /** The kill method attempts to kill an NPC in the player's current location.
   * If the player has a "weapon" and the NPC is alive, then the NPC is killed and the player's wanted level increases.
   * If the player has a "weapon" but the NPC is already dead, the method returns a message indicating as much.
   * If the player does not have a "weapon," the method returns a message indicating that the player needs one to kill the NPC. */
  def kill() =
    var result = ""
    if this.has("weapon") && !this.currentLocation.nonPlayerCharacters.dead then
      this.currentLocation.nonPlayerCharacters.dead = true
      result = "Walter: I killed the person!!!"
      if this.currentLocation.nonPlayerCharacters.firstName() == "Jesse" then
        this.killCounts += 5
      else
        this.killCounts += 1

      if this.killCounts >= 5 then
        this.arrested = true
      if this.currentLocation.nonPlayerCharacters.value() == "money" then
        this.location.addItem(Item("money", "Nothing interesting."))
        this.get("money")
    else if this.has("weapon") && this.currentLocation.nonPlayerCharacters.dead then
      result = "Walter: Rest in Peace"
    else
      result = "Walter: I can't kill with my hands. They are too weak. I need to get a weapon!"
    result +"\n" + wantedLevel()


  /** The purchase method attempts to purchase chemicals from an NPC in the player's current location.
   * If the player has "money" and is in a location where the NPC sells chemicals, then the player receives the chemicals.
   * If these conditions are not met, the method returns a message indicating as such. */
  def purchase() =
    var result = ""
    if this.has("money") && this.currentLocation.nonPlayerCharacters.value() == "chemicals" then
      this.location.addItem(Item("chemicals", "KNO3 and sugar. Candy rocket's fuel components."))
      this.get("chemicals")
      result = "Jesse: Thank you for your purchase!"

    else
      result = "Jesse: Umm, I don't sell it for free!"
    result


  /** The cook method allows the player to make the "Solid fuel" if they have the "chemicals" in their inventory and are in the "Lab" area.
   * If the conditions are not met, the method returns an error message. */
  def cook() =
    var result = ""
    if this.has("chemicals") && this.currentLocation.name == "Lab" then
      this.location.addItem(Item("Solid fuel", "Orange, explosive."))
      this.get("Solid fuel")
      result = "Walter: Wow, some hot thing over there. I need to visit Oliver"
    else if this.has("chemicals") && this.currentLocation.name != "Lab" then
      result = "Walter: I should go to Lab to cook here."
    else
      result = "Walter: I don't have chemicals, can't cook..."
    result


  /** The help method returns a string that lists all of the available commands in the game. */
  def help() = "HELP WITH COMMANDS" +
    "\ngo 'direction' - moves the player to selected direction (south, north, east or west)" +
    "\nrest - player relaxes" +
    "\nxyzzy - some rando text" +
    "\nquit - quit the game" +
    "\nget - get items from the area" +
    "\ninventory - check items in your capacity" +
    "\nexamine - examine items" +
    "\ndrop 'item name' - drop items" +
    "\ntalk - talk with non player characters" +
    "\nkill - kill the npc" +
    "\npurchase - buy chemicals in labofarma" +
    "\ncook - make desired fuel" +
    "\nhelp - show available commands"

  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

end Player

