package o1.adventure

class NonPlayers(game: Adventure, name: String, speechFirstMeet: String, speechAfterFirst: String, item: String, specialItem: String):

  var dead = false
  var talkedAlready = false

  /** Returns the first name of the non-player character. */
  def firstName() =
    name


  /** Returns the special item of the non-player character.
   * In this game, there are only two special items: money and chemicals. */
  def value() =
    specialItem


  /** Returns the speech of the non-player character based on the situation:
   * 1) If it's the first meeting and the non-player character is alive, then the non-player character says its first speech.
   * 2) If it's their second meeting, the non-player character changes replics.
   * 3) If the non-player character is dead, then it says "....".
   */
  def say() =
    var result = ""
    if !talkedAlready && !dead then
      this.game.player.location.addItem(Item(item, "Nothing interesting."))
      this.game.player.get(item)
      talkedAlready = true
      result = speechFirstMeet

    else if talkedAlready && !dead then
      result = speechAfterFirst

    else if dead then
      result = "......"

    result