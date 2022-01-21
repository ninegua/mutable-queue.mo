/*
 * A mutable queue with pushFront and pushBack, but only popFront.
 *
 */
import Iter "mo:base/Iter";
import Nat64 "mo:base/Nat64";
import Option "mo:base/Option";

import Matchers "mo:matchers/Matchers";
import T "mo:matchers/Testable";
import Suite "mo:matchers/Suite";

module Queue {
  public type List<T> = {
    item: T;
    var next: ?List<T>;
  };

  public type Queue<T> = {
    var size: Nat64;
    var first: ?List<T>;
    var last: ?List<T>;
  };

  public func empty<T>() : Queue<T> {
    { var size = 0; var first = null; var last = null; }
  };

  public func pushBack<T>(q: Queue<T>, item: T) : Queue<T> {
    let list : List<T> = { item = item; var next = null };
    switch (q.first, q.last) {
      case (?first, ?last) {
        last.next := ?list;
        q.last := last.next;
      };
      case (_, _) {
        q.first := ?list;
        q.last := q.first;
      }
    };
    q.size := q.size + 1;
    q
  };

  public func pushFront<T>(item: T, q: Queue<T>) : Queue<T> {
    let list : List<T> = { item = item; var next = null };
    switch (q.first, q.last) {
      case (?first, ?last) {
        list.next := ?first;
        q.first := ?list;
      };
      case (_, _) {
        q.first := ?list;
        q.last := q.first;
      }
    };
    q.size := q.size + 1;
    q
  };

  public func popFront<T>(q: Queue<T>) : ?T {
    switch (q.first, q.last) {
      case (?first, ?last) {
        q.size := q.size - 1;
        let item = first.item;
        if (q.size == 0) {
          q.first := null;
          q.last := null;
        } else {
          q.first := first.next;
        };
        ?item
      };
      case (_, _) null;
    }
  };

  public func rotate<T>(q: Queue<T>) : Queue<T> {
    switch (q.first, q.last) {
      case (?first, ?last) {
        if (q.size > 1) {
          q.first := first.next;
          first.next := null;
          last.next := ?first;
          q.last := last.next;
        }
      };
      case (_, _) ();
    };
    q
  };

  public func remove<T>(q: Queue<T>, eq: T -> Bool) : Queue<T> {
    ignore removeOne(q, eq);
    q
  };

  public func removeOne<T>(q: Queue<T>, eq: T -> Bool) : ?T {
    switch (q.first, q.last) {
      case (?first, ?last) {
        if (eq(first.item)) {
          q.first := first.next;
          if (Option.isNull(q.first)) {
            q.last := null;
          };
          q.size := q.size - 1;
          ?first.item;
        } else {
          var prev = first;
          label L loop {
            switch (prev.next) {
              case null { break L };
              case (?next) {
                if (eq(next.item)) {
                  prev.next := next.next;
                  if (Option.isNull(prev.next)) {
                    q.last := ?prev;
                  };
                  q.size := q.size - 1;
                  return ?next.item;
                };
                prev := next;
              }
            }
          };
          null
        }
      };
      case _ { null };
    }
  };

  public func first<T>(q: Queue<T>) : ?T {
    Option.map(q.first, func (x: List<T>) : T { x.item })
  };

  public func last<T>(q: Queue<T>) : ?T {
    Option.map(q.last, func (x: List<T>) : T { x.item })
  };

  public func make<T>(item: T) : Queue<T> {
    let q = empty<T>();
    pushBack<T>(q, item)
  };

  public func size<T>(q: Queue<T>) : Nat {
    Nat64.toNat(q.size)
  };

  public func toIter<T>(q: Queue<T>) : Iter.Iter<T> {
    var cursor = q.first;
    func next() : ?T {
      switch (cursor) {
        case null null;
        case (?list) {
          let item = list.item;
          cursor := list.next;
          ?item
        }
      }
    };
    { next = next }
  };

  public func find<T>(q: Queue<T>, f: T -> Bool) : ?T {
    for (item in toIter(q)) {
      if (f(item)) { return ?item }
    };
    null
  };

  public func fold<T, V>(q: Queue<T>, init: V, acc: (V, T) -> V) : V {
    var sum = init;
    for (item in toIter(q)) {
      sum := acc(sum, item);
    };
    sum
  };

  public func fromIter<T>(iter: Iter.Iter<T>) : Queue<T> {
    let q = empty<T>();
    for (item in iter) {
      ignore pushBack(q, item);
    };
    q
  };

  public func fromArray<T>(arr: [T]) : Queue<T> {
    fromIter(Iter.fromArray(arr))
  };

  public func toArray<T>(q: Queue<T>) : [T] {
    Iter.toArray<T>(toIter(q))
  };

  public func map<A, B>(inp: Queue<A>, f: A -> B) : Queue<B> {
    let out = Queue.empty<B>();
    for (x in toIter(inp)) {
      ignore pushBack(out, f(x));
    };
    out
  };

  type Suite = Suite.Suite;
  public func test() : Suite {
    func wellformed<A>(q: Queue<A>) : Queue<A> {
      switch (q.first, q.last) {
        case (?first, ?last) {
          assert(size(q) > 0);
          if (size(q) == 1) {
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
            assert(n == size(q));
          }
        };
        case (?_, _) { assert(false) };
        case (_, ?_) { assert(false) };
        case (_, _)  { assert(size(q) == 0) };
      };
      q
    };

    func toArray_<A>(q: Queue<A>) : [A] {
      toArray(wellformed(q))
    };

    let to_array_test =
      Suite.suite("toArray", [
        Suite.test("empty",
          toArray_(empty<Nat>()),
          Matchers.equals(T.array<Nat>(T.natTestable, []))),
        Suite.test("singleton",
          toArray_(make<Nat>(1)),
          Matchers.equals(T.array<Nat>(T.natTestable, [1]))),
        Suite.test("multiple",
          toArray_(pushFront(0, pushBack(make<Nat>(1), 2))),
          Matchers.equals(T.array<Nat>(T.natTestable, [0, 1, 2]))),
    ]);

    let size_test =
      Suite.suite("size", [
        Suite.test("empty",
          size(empty<Nat>()),
          Matchers.equals(T.nat(0))),
        Suite.test("singleton",
          size(make<Nat>(1)),
          Matchers.equals(T.nat(1))),
        Suite.test("multiple",
          size(pushFront(0, pushBack(make<Nat>(1), 2))),
          Matchers.equals(T.nat(3))),
        Suite.test("fromArray",
          size(fromArray<Nat>([1,2,3,4])),
          Matchers.equals(T.nat(4))),
        Suite.test("popFront",
          size(do { let q = make<Nat>(1); ignore popFront(q); q }),
          Matchers.equals(T.nat(0))),
        Suite.test("pushFront",
          size(pushFront(0, make<Nat>(1))),
          Matchers.equals(T.nat(2))),
        Suite.test("pushBack",
          size(pushBack(make<Nat>(1), 2)),
          Matchers.equals(T.nat(2))),
        Suite.test("rotate",
          size(rotate(fromArray<Nat>([1,2,3,4]))),
          Matchers.equals(T.nat(4))),
    ]);

    func eq(a: Nat) : Nat -> Bool { func (b: Nat) : Bool { a == b } };
    let removal_test =
      Suite.suite("removal", [
        Suite.test("removeOne/empty",
          removeOne<Nat>(empty<Nat>(), eq(10)),
          Matchers.equals(T.optional<Nat>(T.natTestable, null))),
        Suite.test("removeOne/one",
          removeOne<Nat>(make<Nat>(10), eq(10)),
          Matchers.equals(T.optional(T.natTestable, ?10))),
        Suite.test("removeOne/one_1",
          removeOne<Nat>(make<Nat>(0), eq(10)),
          Matchers.equals(T.optional<Nat>(T.natTestable, null))),
        Suite.test("removeOne/two",
          removeOne<Nat>(fromArray<Nat>([3,10]), eq(3)),
          Matchers.equals(T.optional(T.natTestable, ?3))),
        Suite.test("removeOne/two_1",
          removeOne<Nat>(fromArray<Nat>([3,10]), eq(10)),
          Matchers.equals(T.optional(T.natTestable, ?10))),
        Suite.test("removeOne/two_2",
          removeOne<Nat>(fromArray<Nat>([3,10]), eq(0)),
          Matchers.equals(T.optional<Nat>(T.natTestable, null))),
        Suite.test("remove/empty",
          toArray_(remove<Nat>(empty<Nat>(), eq(10))),
          Matchers.equals(T.array<Nat>(T.natTestable, []))),
        Suite.test("remove/one",
          toArray_(remove<Nat>(make<Nat>(10), eq(10))),
          Matchers.equals(T.array<Nat>(T.natTestable, []))),
        Suite.test("remove/one_1",
          toArray_(remove<Nat>(make<Nat>(0), eq(10))),
          Matchers.equals(T.array<Nat>(T.natTestable, [0]))),
        Suite.test("remove/two",
          toArray_(remove<Nat>(fromArray<Nat>([3,10]), eq(3))),
          Matchers.equals(T.array<Nat>(T.natTestable, [10]))),
        Suite.test("remove/two_1",
          toArray_(remove<Nat>(fromArray<Nat>([3,10]), eq(10))),
          Matchers.equals(T.array<Nat>(T.natTestable, [3]))),
        Suite.test("remove/two_2",
          toArray_(remove<Nat>(fromArray<Nat>([3,10]), eq(0))),
          Matchers.equals(T.array<Nat>(T.natTestable, [3, 10]))),
     ]);

    Suite.suite("Queue", [ to_array_test, size_test, removal_test ]);
  }

}

