package tsp

class GreedyStart(customers: Array[Array[Double]])
    extends TravelPlan(customers) {
  tour = customers.indices.toArray
}
