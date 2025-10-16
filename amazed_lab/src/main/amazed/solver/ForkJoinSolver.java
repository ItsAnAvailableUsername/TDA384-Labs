package amazed.solver;

import amazed.maze.Maze;

import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.RecursiveTask;

// SequentialSolver defines multiple unnecessary variables.
// By extending RecursiveTask<List<Integer>> directly we can simplify the implementation.
public class ForkJoinSolver extends RecursiveTask<List<Integer>> {
    // The ForkJoinSolver instances share only the same instances of maze and visited.
    final private Maze maze;
    final private Set<Integer> visited;
    final private int start;

    public ForkJoinSolver(final Maze maze, final int ignoredForkAfter) {
        this(maze, new ConcurrentSkipListSet<>(), maze.start());
    }

    private ForkJoinSolver(final Maze maze, final Set<Integer> visited, final int start) {
        this.maze = maze;
        this.visited = visited;
        this.start = start;
    }

    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    // As far as Iwe can tell, the instructions never specified that the players *have* to move anywhere, so they don't.
    // Instead, they simply perform asexual reproduction and wait for their kids to do everything for them.
    // Question to teachers: is there a reason why parallelSearch is its own method rather than the body of compute?
    private List<Integer> parallelSearch() {
        maze.newPlayer(start);
        if (maze.hasGoal(start)) {
            return new LinkedList<>(Collections.singletonList(start));
        }
        visited.add(start);
        for (final int neighbor : maze.neighbors(start)) {
            if (visited.contains(neighbor)) {
                continue;
            }
            final ForkJoinSolver child = new ForkJoinSolver(maze, visited, neighbor);
            child.fork();
            final List<Integer> result = child.join();
            if (result != null) {
                result.addFirst(start);
                return result;
            }
        }
        return null;
    }
}