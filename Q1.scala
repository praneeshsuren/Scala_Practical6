case class Product(name: String, quantity: Int, price: Double)

type Inventory = Map[Int, Product]

def retrieveAllProductNames(inventory: Inventory): List[String] = {
  inventory.values.map(_.name).toList
}

def calculateTotalValue(inventory: Inventory): Double = {
  inventory.values.map(product => product.quantity * product.price).sum
}

def isInventoryEmpty(inventory: Inventory): Boolean = {
  inventory.isEmpty
}

def mergeInventories(inventory1: Inventory, inventory2: Inventory): Inventory = {
  (inventory1.keySet ++ inventory2.keySet).map { id =>
    val product1 = inventory1.get(id)
    val product2 = inventory2.get(id)

    id -> (for {
      p1 <- product1
      p2 <- product2
    } yield Product(
      p1.name,
      p1.quantity + p2.quantity,
      math.max(p1.price, p2.price)
    )).getOrElse(product1.getOrElse(product2.get))
  }.toMap
}

def checkProductExists(inventory: Inventory, productId: Int): Unit = {
  inventory.get(productId) match {
    case Some(product) =>
      println(
        s"Product ID: $productId, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}"
      )
    case None =>
      println(s"Product with ID $productId does not exist.")
  }
}

@main def main1(): Unit = {
  val inventory1: Inventory = Map(
    101 -> Product("MI", 10, 2.5),
    102 -> Product("Apple", 5, 3.0),
    103 -> Product("Nothing", 2, 1.75)
  )

  val inventory2: Inventory = Map(
    102 -> Product("Apple", 7, 2.8),
    104 -> Product("Samsung", 4, 4.0)
  )

  println(
    "All product names in inventory1: " + retrieveAllProductNames(inventory1).mkString(", ")
  )

  println(
    "Total value of all products in inventory1: " + calculateTotalValue(inventory1)
  )

  println("Is inventory1 empty? " + isInventoryEmpty(inventory1))

  val mergedInventory = mergeInventories(inventory1, inventory2)

  println("Merged inventory: ")
  mergedInventory.foreach { case (id, product) =>
    println(
      s"ID: $id, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}"
    )
  }

  checkProductExists(inventory1, 102)
}