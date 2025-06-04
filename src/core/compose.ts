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

export async function pipe<T>(
  source: AsyncGenerator<T>,
  ...transforms: Array<(input: AsyncGenerator<any>) => AsyncGenerator<any>>
): Promise<void> {
  let current: AsyncGenerator<any> = source;
  
  for (const transform of transforms) {
    current = transform(current);
  }
  
  // Consume the final generator
  for await (const _ of current) {
    // Pipeline complete
  }
}