package io.iohk.ethereum.common

/**
  * Interface to represent a key-value structure
  */
trait SimpleMap[K, V, T <: SimpleMap[K, V, T]] {

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  def get(key: K): Option[V]

  /**
    * This function inserts a (key-value) pair into the trie. If the key is already asociated with another value it is updated.
    *
    * @param key
    * @param value
    * @return New trie with the (key-value) pair inserted.
    */
  def put(key: K, value: V): T = update(Seq.empty, Seq(key -> value))

  /**
    * This function inserts a (key-value) pair into the trie. If the key is already asociated with another value it is updated.
    *
    * @param kv to insert
    * @return New trie with the (key-value) pair inserted.
    */
  def +(kv: (K, V)): T = put(kv._1, kv._2)


  /**
    * This function deletes a (key-value) pair from the trie. If no (key-value) pair exists with the passed trie then there's no effect on it.
    *
    * @param key
    * @return New trie with the (key-value) pair associated with the key passed deleted from the trie.
    */
  def remove(key: K): T = update(Seq(key), Seq.empty)

  /**
    * This function deletes a (key-value) pair from the trie. If no (key-value) pair exists with the passed trie then there's no effect on it.
    *
    * @param key
    * @return New trie with the (key-value) pair associated with the key passed deleted from the trie.
    */
  def -(key: K): T = remove(key)


  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  def update(toRemove: Seq[K], toUpsert: Seq[(K, V)]): T

}
