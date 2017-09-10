package app.domain


case class Pet(
                id: Long,
                name: String,
                breed: String,
                age: Int
              )

case class Person(
                   id: Long,
                   name: String,
                   age: Int,
                   children: Seq[Person],
                   pet: Option[Pet]
                 )
