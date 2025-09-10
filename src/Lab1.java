import TSim.*;
import java.util.*;
import java.util.concurrent.Semaphore;

public class Lab1 {
  public Lab1(int speed1, int speed2) {
    TrackMap.initialize();

    Track startTrack1 = TrackMap.getTrackById(7); // Train 1 starts at station B
    Track startTrack2 = TrackMap.getTrackById(1); // Train 2 starts at station A

    Train train1 = new Train(1, speed1, Direction.TowardsA, startTrack1);
    Train train2 = new Train(2, speed2, Direction.TowardsB, startTrack2);

    train1.start();
    train2.start();
  }
}

class Train extends Thread {
  private int id;
  private int speed;
  private Track currentTrack;
  private Track previousTrack;
  private Direction travelDirection;
  private TSimInterface tsi = TSimInterface.getInstance();

  public Train(int id, int speed, Direction initialDirection, Track startTrack) {
    this.id = id;
    this.speed = speed;
    this.travelDirection = initialDirection;
    this.currentTrack = startTrack;
    this.previousTrack = null;

    // Acquire the starting track
    try {
      startTrack.semaphore.acquire();
    } catch (InterruptedException e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

  @Override
  public void run() {
    try {
      tsi.setSpeed(id, speed);
      while (true) {
        SensorEvent event = tsi.getSensor(this.id);
        handleSensorEvent(event);
      }
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

  private void handleSensorEvent(SensorEvent event) throws Exception {
    Coordinate sensor = new Coordinate(event.getXpos(), event.getYpos());

    // Check if this is a cross section sensor
    CrossSection crossSection = TrackMap.getCrossSection(sensor);
    if (crossSection != null) {
      handleCrossSectionEvent(event, crossSection);
      return;
    }

    // Handle regular track sensors
    if (event.getStatus() == SensorEvent.ACTIVE) {
      if (sensor.equals(getDestinationSensor())) {
        // Reached destination sensor - try to acquire next track
        tsi.setSpeed(id, 0);
        Track nextTrack = currentTrack.acquireNextTrack(travelDirection);

        if (nextTrack != null) {
          // Successfully acquired next track
          previousTrack = currentTrack;
          currentTrack = nextTrack;
        } else {
          // Reached station - need to turn around
          waitAndTurnBack();
        }
        tsi.setSpeed(id, speed);
      }
    } else if (event.getStatus() == SensorEvent.INACTIVE) {
      if (sensor.equals(getOriginSensor())) {
        // Left origin sensor - release previous track
        if (previousTrack != null) {
          previousTrack.semaphore.release();
          previousTrack = null;
        }
      }
    }
  }

  private void handleCrossSectionEvent(SensorEvent event, CrossSection crossSection) throws Exception {
    boolean hasReachedEnd = (travelDirection == crossSection.travelDirection);
    boolean hasReachedStart = (travelDirection != crossSection.travelDirection);

    if (event.getStatus() == SensorEvent.ACTIVE && hasReachedStart) {
      // Entering cross section - acquire semaphore
      tsi.setSpeed(id, 0);
      crossSection.semaphore.acquire();
      tsi.setSpeed(id, speed);
    } else if (event.getStatus() == SensorEvent.INACTIVE && hasReachedEnd) {
      // Leaving cross section - release semaphore
      crossSection.semaphore.release();
    }
  }

  private Coordinate getDestinationSensor() {
    return (travelDirection == Direction.TowardsA) ?
            currentTrack.sensorClosedToA : currentTrack.sensorClosedToB;
  }

  private Coordinate getOriginSensor() {
    return (travelDirection == Direction.TowardsA) ?
            currentTrack.sensorClosedToB : currentTrack.sensorClosedToA;
  }

  private void waitAndTurnBack() throws Exception {
    int waitTime = 1000 + (20 * Math.abs(speed));
    Thread.sleep(waitTime);

    // Change direction
    travelDirection = (travelDirection == Direction.TowardsA) ?
            Direction.TowardsB : Direction.TowardsA;
    speed = -speed;
  }
}

class Track {
  public final int id;
  public final Coordinate sensorClosedToA;
  public final Coordinate sensorClosedToB;
  public final Semaphore semaphore;
  public final List<Connection> connectionsTowardsA;
  public final List<Connection> connectionsTowardsB;

  public Track(int id, Coordinate sensorA, Coordinate sensorB) {
    this.id = id;
    this.sensorClosedToA = sensorA;
    this.sensorClosedToB = sensorB;
    this.semaphore = new Semaphore(1);
    this.connectionsTowardsA = new ArrayList<>();
    this.connectionsTowardsB = new ArrayList<>();
  }

  public void addConnection(Track toTrack, Direction direction, Coordinate switchPos, int switchDir) {
    Connection connection = new Connection(toTrack, switchPos, switchDir);
    if (direction == Direction.TowardsA) {
      connectionsTowardsA.add(connection);
    } else {
      connectionsTowardsB.add(connection);
    }
  }

  public Track acquireNextTrack(Direction direction) throws Exception {
    List<Connection> candidates = (direction == Direction.TowardsA) ?
            connectionsTowardsA : connectionsTowardsB;

    // If no connections, we've reached a station
    if (candidates.isEmpty()) {
      return null;
    }

    // Try to acquire tracks in order of preference
    for (int i = 0; i < candidates.size(); i++) {
      Connection connection = candidates.get(i);
      Track nextTrack = connection.toTrack;

      if (i == candidates.size() - 1) {
        // Last option: blocking wait
        nextTrack.semaphore.acquire();
        connection.activateSwitch();
        return nextTrack;
      } else if (nextTrack.semaphore.tryAcquire()) {
        // Non-blocking attempt succeeded
        connection.activateSwitch();
        return nextTrack;
      }
    }

    return null;
  }
}

class Connection {
  public final Track toTrack;
  public final Coordinate switchPosition;
  public final int switchDirection;

  public Connection(Track toTrack, Coordinate switchPos, int switchDir) {
    this.toTrack = toTrack;
    this.switchPosition = switchPos;
    this.switchDirection = switchDir;
  }

  public void activateSwitch() throws CommandException {
    if (switchPosition != null) {
      TSimInterface.getInstance().setSwitch(
              switchPosition.x, switchPosition.y, switchDirection
      );
    }
  }
}

class CrossSection {
  public final Semaphore semaphore;
  public final Direction travelDirection;

  public CrossSection(Direction direction, Semaphore semaphore) {
    this.travelDirection = direction;
    this.semaphore = semaphore;
  }
}

class TrackMap {
  private static Map<Integer, Track> tracks = new HashMap<>();
  private static Map<Coordinate, CrossSection> crossSections = new HashMap<>();

  public static void initialize() {
    // Cross sections: shared semaphore for the intersection
    Semaphore crossSemaphore = new Semaphore(1);
    CrossSection crossTowardsA1 = new CrossSection(Direction.TowardsA, crossSemaphore);
    CrossSection crossTowardsA2 = new CrossSection(Direction.TowardsA, crossSemaphore);
    CrossSection crossTowardsB1 = new CrossSection(Direction.TowardsB, crossSemaphore);
    CrossSection crossTowardsB2 = new CrossSection(Direction.TowardsB, crossSemaphore);

    crossSections.put(new Coordinate(10, 7), crossTowardsA1);
    crossSections.put(new Coordinate(9, 8), crossTowardsA2);
    crossSections.put(new Coordinate(6, 7), crossTowardsB1);
    crossSections.put(new Coordinate(8, 5), crossTowardsB2);

    // Create and store all tracks
    Track track1 = new Track(1, new Coordinate(16, 11), new Coordinate(5, 11));
    Track track2 = new Track(2, new Coordinate(16, 13), new Coordinate(3, 13));
    Track track3 = new Track(3, new Coordinate(1, 11), new Coordinate(2, 9));
    Track track4 = new Track(4, new Coordinate(6, 9), new Coordinate(13, 9));
    Track track5 = new Track(5, new Coordinate(6, 10), new Coordinate(13, 10));
    Track track6 = new Track(6, new Coordinate(17, 9), new Coordinate(19, 7));
    Track track7 = new Track(7, new Coordinate(15, 7), new Coordinate(16, 3));
    Track track8 = new Track(8, new Coordinate(15, 8), new Coordinate(16, 5));

    tracks.put(1, track1);
    tracks.put(2, track2);
    tracks.put(3, track3);
    tracks.put(4, track4);
    tracks.put(5, track5);
    tracks.put(6, track6);
    tracks.put(7, track7);
    tracks.put(8, track8);

    // Define switch positions
    Coordinate switch1 = new Coordinate(3, 11);
    Coordinate switch2 = new Coordinate(4, 9);
    Coordinate switch3 = new Coordinate(15, 9);
    Coordinate switch4 = new Coordinate(17, 7);

    // Set up track connections
    // TowardsB direction connections
    track1.addConnection(track3, Direction.TowardsB, switch1, TSimInterface.SWITCH_LEFT);
    track2.addConnection(track3, Direction.TowardsB, switch1, TSimInterface.SWITCH_RIGHT);
    track3.addConnection(track4, Direction.TowardsB, switch2, TSimInterface.SWITCH_LEFT);
    track3.addConnection(track5, Direction.TowardsB, switch2, TSimInterface.SWITCH_RIGHT);
    track4.addConnection(track6, Direction.TowardsB, switch3, TSimInterface.SWITCH_RIGHT);
    track5.addConnection(track6, Direction.TowardsB, switch3, TSimInterface.SWITCH_LEFT);
    track6.addConnection(track7, Direction.TowardsB, switch4, TSimInterface.SWITCH_RIGHT);
    track6.addConnection(track8, Direction.TowardsB, switch4, TSimInterface.SWITCH_LEFT);

    // TowardsA direction connections
    track3.addConnection(track1, Direction.TowardsA, switch1, TSimInterface.SWITCH_LEFT);
    track3.addConnection(track2, Direction.TowardsA, switch1, TSimInterface.SWITCH_RIGHT);
    track4.addConnection(track3, Direction.TowardsA, switch2, TSimInterface.SWITCH_LEFT);
    track5.addConnection(track3, Direction.TowardsA, switch2, TSimInterface.SWITCH_RIGHT);
    track6.addConnection(track4, Direction.TowardsA, switch3, TSimInterface.SWITCH_RIGHT);
    track6.addConnection(track5, Direction.TowardsA, switch3, TSimInterface.SWITCH_LEFT);
    track7.addConnection(track6, Direction.TowardsA, switch4, TSimInterface.SWITCH_RIGHT);
    track8.addConnection(track6, Direction.TowardsA, switch4, TSimInterface.SWITCH_LEFT);
  }

  public static Track getTrackById(int id) {
    return tracks.get(id);
  }

  public static CrossSection getCrossSection(Coordinate sensor) {
    return crossSections.get(sensor);
  }
}

enum Direction {
  TowardsA,
  TowardsB
}

class Coordinate {
  public final int x;
  public final int y;

  public Coordinate(int x, int y) {
    this.x = x;
    this.y = y;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    Coordinate that = (Coordinate) obj;
    return x == that.x && y == that.y;
  }

  @Override
  public int hashCode() {
    return x * 100 + y;
  }

  @Override
  public String toString() {
    return "(" + x + "," + y + ")";
  }
}