/*
 * A mutable queue with pushFront and pushBack, but only popFront.
 *
 */
import Iter "mo:core/Iter";
import Nat64 "mo:core/Nat64";
import Option "mo:core/Option";

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
      case (?_first, ?last) {
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
      case (?first, ?_last) {
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
      case (?first, ?_last) {
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
      case (?first, ?_last) {
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

}

