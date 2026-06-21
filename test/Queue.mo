import Queue "../src/Queue";
import Option "mo:base/Option";

import Matchers "mo:matchers/Matchers";
import T "mo:matchers/Testable";
import Suite "mo:matchers/Suite";

type Queue<T> = Queue.Queue<T>;
type Suite = Suite.Suite;

func test() : Suite {
  func wellformed<A>(q: Queue<A>) : Queue<A> {
    switch (q.first, q.last) {
      case (?first, ?last) {
        assert(Queue.size(q) > 0);
        if (Queue.size(q) == 1) {
          assert(Option.isNull(first.next));
          assert(Option.isNull(last.next));
        } else {
          var n = 0;
          var p = first;
          label L loop {
            n := n + 1;
            switch (p.next) {
              case null { break L };
              case (?next) { p := next; }
            }
          };
          assert(n == Queue.size(q));
        }
      };
      case (?_, _) { assert(false) };
      case (_, ?_) { assert(false) };
      case (_, _)  { assert(Queue.size(q) == 0) };
    };
    q
  };

  func toArray_<A>(q: Queue<A>) : [A] {
    Queue.toArray(wellformed(q))
  };

  let to_array_test =
    Suite.suite("toArray", [
      Suite.test("empty",
        toArray_(Queue.empty<Nat>()),
        Matchers.equals(T.array<Nat>(T.natTestable, []))),
      Suite.test("singleton",
        toArray_(Queue.make<Nat>(1)),
        Matchers.equals(T.array<Nat>(T.natTestable, [1]))),
      Suite.test("multiple",
        toArray_(Queue.pushFront(0, Queue.pushBack(Queue.make<Nat>(1), 2))),
        Matchers.equals(T.array<Nat>(T.natTestable, [0, 1, 2]))),
  ]);

  let size_test =
    Suite.suite("size", [
      Suite.test("empty",
        Queue.size(Queue.empty<Nat>()),
        Matchers.equals(T.nat(0))),
      Suite.test("singleton",
        Queue.size(Queue.make<Nat>(1)),
        Matchers.equals(T.nat(1))),
      Suite.test("multiple",
        Queue.size(Queue.pushFront(0, Queue.pushBack(Queue.make<Nat>(1), 2))),
        Matchers.equals(T.nat(3))),
      Suite.test("fromArray",
        Queue.size(Queue.fromArray<Nat>([1,2,3,4])),
        Matchers.equals(T.nat(4))),
      Suite.test("popFront",
        Queue.size(do { let q = Queue.make<Nat>(1); ignore Queue.popFront(q); q }),
        Matchers.equals(T.nat(0))),
      Suite.test("pushFront",
        Queue.size(Queue.pushFront(0, Queue.make<Nat>(1))),
        Matchers.equals(T.nat(2))),
      Suite.test("pushBack",
        Queue.size(Queue.pushBack(Queue.make<Nat>(1), 2)),
        Matchers.equals(T.nat(2))),
      Suite.test("rotate",
        Queue.size(Queue.rotate(Queue.fromArray<Nat>([1,2,3,4]))),
        Matchers.equals(T.nat(4))),
  ]);

  func eq(a: Nat) : Nat -> Bool { func (b: Nat) : Bool { a == b } };
  let removal_test =
    Suite.suite("removal", [
      Suite.test("removeOne/empty",
        Queue.removeOne<Nat>(Queue.empty<Nat>(), eq(10)),
        Matchers.equals(T.optional<Nat>(T.natTestable, null))),
      Suite.test("removeOne/one",
        Queue.removeOne<Nat>(Queue.make<Nat>(10), eq(10)),
        Matchers.equals(T.optional(T.natTestable, ?10))),
      Suite.test("removeOne/one_1",
        Queue.removeOne<Nat>(Queue.make<Nat>(0), eq(10)),
        Matchers.equals(T.optional<Nat>(T.natTestable, null))),
      Suite.test("removeOne/two",
        Queue.removeOne<Nat>(Queue.fromArray<Nat>([3,10]), eq(3)),
        Matchers.equals(T.optional(T.natTestable, ?3))),
      Suite.test("removeOne/two_1",
        Queue.removeOne<Nat>(Queue.fromArray<Nat>([3,10]), eq(10)),
        Matchers.equals(T.optional(T.natTestable, ?10))),
      Suite.test("removeOne/two_2",
        Queue.removeOne<Nat>(Queue.fromArray<Nat>([3,10]), eq(0)),
        Matchers.equals(T.optional<Nat>(T.natTestable, null))),
      Suite.test("remove/empty",
        toArray_(Queue.remove<Nat>(Queue.empty<Nat>(), eq(10))),
        Matchers.equals(T.array<Nat>(T.natTestable, []))),
      Suite.test("remove/one",
        toArray_(Queue.remove<Nat>(Queue.make<Nat>(10), eq(10))),
        Matchers.equals(T.array<Nat>(T.natTestable, []))),
      Suite.test("remove/one_1",
        toArray_(Queue.remove<Nat>(Queue.make<Nat>(0), eq(10))),
        Matchers.equals(T.array<Nat>(T.natTestable, [0]))),
      Suite.test("remove/two",
        toArray_(Queue.remove<Nat>(Queue.fromArray<Nat>([3,10]), eq(3))),
        Matchers.equals(T.array<Nat>(T.natTestable, [10]))),
      Suite.test("remove/two_1",
        toArray_(Queue.remove<Nat>(Queue.fromArray<Nat>([3,10]), eq(10))),
        Matchers.equals(T.array<Nat>(T.natTestable, [3]))),
      Suite.test("remove/two_2",
        toArray_(Queue.remove<Nat>(Queue.fromArray<Nat>([3,10]), eq(0))),
        Matchers.equals(T.array<Nat>(T.natTestable, [3, 10]))),
   ]);

  Suite.suite("Queue", [ to_array_test, size_test, removal_test ]);
};

Suite.run(test())
