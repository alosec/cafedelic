/**
 * WTE Pipeline Runner
 * Executes Watch-Transform-Execute pipelines
 */

import { WTE } from './wte.js';

export async function runPipeline<T, A>(wte: WTE<T, A>): Promise<void> {
  for await (const event of wte.watch()) {
    const action = wte.transform(event);
    if (action !== null) {
      await wte.execute(action);
    }
  }
}