package objsets

import TweetReader._

/**
  * A class to represent tweets.
  */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

  // TODO : for comparing tweets
  def > (that : Tweet) : Tweet = if (retweets >= that.retweets) this else that
}

/**
  * This represents a set of objects of type `Tweet` in the form of a binary search
  * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
  * invariant which always holds: for every branch `b`, all elements in the left
  * subtree are smaller than the tweet at `b`. The elements in the right subtree are
  * larger.
  *
  * Note that the above structure requires us to be able to compare two tweets (we
  * need to be able to say which of two tweets is larger, or if they are equal). In
  * this implementation, the equality / order of tweets is based on the tweet's text
  * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
  * text from different users.
  *
  *
  * The advantage of representing sets as binary search trees is that the elements
  * of the set can be found quickly. If you want to learn more you can take a look
  * at the Wikipedia page [1], but this is not necessary in order to solve this
  * assignment.
  *
  * [1] http://en.wikipedia.org/wiki/Binary_search_tree
  */
abstract class TweetSet {

  // helper to avoid instance checking
  def isEmpty :  Boolean

  /**
    * This method takes a predicate and returns a subset of all the elements
    * in the original set for which the predicate is true.
    *
    * Question: Can we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  // Empty and NonEmpty call their own filterAcc.  initial results are Empty
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
    * This is a helper method for `filter` that propagates the accumulated tweets.
    */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  // Empty and NonEmpty call their own union.
  def union(that: TweetSet): TweetSet

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  // Empty and NonEmpty call their own mostRetweeted
  def mostRetweeted: Tweet

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  // Empty and NonEmpty call their own descendingByRetweet
  def descendingByRetweet: TweetList

  /**
    * The following methods are already implemented
    */

  /**
    * Returns a new `TweetSet` which contains all elements of this set, and the
    * the new element `tweet` in case it does not already exist in this set.
    *
    * If `this.contains(tweet)`, the current set is returned.
    */
  def incl(tweet: Tweet): TweetSet

  /**
    * Returns a new `TweetSet` which excludes `tweet`.
    */
  def remove(tweet: Tweet): TweetSet

  /**
    * Tests if `tweet` exists in this `TweetSet`.
    */
  def contains(tweet: Tweet): Boolean

  /**
    * This method takes a function and applies it to every element in the set.
    */
  def foreach(f: Tweet => Unit): Unit
}

// TODO
class Empty extends TweetSet {

  // helper check
  def isEmpty : Boolean = true

  // any filter returns the existing accumulation
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  // any union returns that
  def union(that : TweetSet) = that

  // any mostRetweeted is invalid - throw Exception
  def mostRetweeted : Tweet = throw new java.util.NoSuchElementException("Empty TweetSet")

  // any descent returns TweetList, so Nil is useful
  def descendingByRetweet : TweetList = Nil

  /**
    * The following methods are already implemented
    */
  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  // helper
  def isEmpty : Boolean = false

  // TODO
  // if we match on filter, need to add to accumulator
  // then the action is the same: = check right, accumulating results
  // then check left, adding to accumulator
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val accp = if (p(elem)) acc.incl(elem) else acc
    val rightp = right.filterAcc(p, accp)
    left.filterAcc(p, rightp)
  }

  // TODO
  // union right and that
  // union result with ourselves
  // union result with left
  def union(that : TweetSet) : TweetSet = {
    val rightu = right.union(that)
    val eu = rightu.incl(elem)
    left.union(eu)

    // wow
    //left.union(right.union(that).incl(elem))
  }

  // TODO
  // if left and right are empty, we're it
  // otherwise, if left is empty, compare ourselves against right most tweeted
  // otherwise, if right is empty, compare ourselves against left most tweeted
  // otherwise,  we need a threeway compare
  def mostRetweeted : Tweet = {
    if (left.isEmpty & right.isEmpty) elem
    else if (left.isEmpty) {
      elem > right.mostRetweeted
    } else if (right.isEmpty) {
      elem > left.mostRetweeted
    } else {
      elem > (left.mostRetweeted > right.mostRetweeted)
    }
  }

  // TODO
  // use the hint, Luke
  // add most retweeted to head of list, then remove it and recurse on tail
  def descendingByRetweet : TweetList = {
    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)
  }

  /**
    * The following methods are already implemented
    */
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

}

trait TweetList {
  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  // TODO
  // exists takes an A and returns a Boolean.  In this case, is any list member in the tweet text?
  // filter takes a tweet and returns Boolean.  Does the tweet make the text
  lazy val googleTweets: TweetSet = allTweets.filter(t => google.exists(s => t.text.contains(s)))
  lazy val appleTweets: TweetSet = allTweets.filter(t => apple.exists(s => t.text.contains(s)))

  /**
    * A list of all tweets mentioning a keyword from either apple or google,
    * sorted by the number of retweets.
    */
  // TODO
  // union google and apple and then descend
  lazy val trending: TweetList = (googleTweets.union(appleTweets)).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
