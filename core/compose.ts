/**
 * Pipeline Composition Utilities
 * Helpers for building complex pipelines
 */

import { WTE, WatcherFn, TransformFn, ExecutorFn } from './wte.js';

export function compose<T, A>(
  watch: WatcherFn<T>,
  transform: TransformFn<T, A>,
  execute: ExecutorFn<A>
): WTE<T, A> {
  return { watch, transform, execute };
}

export function chain<T, M, A>(
  transform1: TransformFn<T, M>,
  transform2: TransformFn<M, A>
): TransformFn<T, A> {
  return (event: T) => {
    const middle = transform1(event);
    return middle !== null ? transform2(middle) : null;
  };
}