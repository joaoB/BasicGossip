package example.basicGossip.protocols;

import java.util.Random;

import peersim.cdsim.CDProtocol;
import peersim.config.Configuration;
import peersim.config.FastConfig;
import peersim.core.Linkable;
import peersim.core.Node;
import peersim.edsim.EDProtocol;
import peersim.transport.Transport;
import peersim.vector.SingleValueHolder;
import example.basicGossip.Info;
import example.basicGossip.Usernode;

/**
 * Event driven version of epidemic averaging.
 */
public class BasicGossip extends SingleValueHolder implements CDProtocol,
		EDProtocol {

	public static int fanout;
	public static int networkSize;
	public static int cycles;
	protected int info;

	// --------------------------------------------------------------------------
	// Initialization
	// --------------------------------------------------------------------------

	/**
	 * @param prefix
	 *            string prefix for config properties
	 */
	public BasicGossip(String prefix) {
		super(prefix);
		networkSize = (Configuration.getInt("network.size", 1));
		cycles = (Configuration.getInt("CYCLES", 1));
		fanout = (int) Math.log(networkSize) + 1;
	}

	// --------------------------------------------------------------------------
	// methods
	// --------------------------------------------------------------------------

	/**
	 * This is the standard method the define periodic activity. The frequency
	 * of execution of this method is defined by a
	 * {@link peersim.edsim.CDScheduler} component in the configuration.
	 */
	public void nextCycle(Node node, int pid) {
		// System.out.println("PID " + pid);
		// streamer
		if (node.getIndex() == 0) {
			System.out.println("streamer generating " + info);
			Random r = new Random();
			for (int i = 0; i < fanout; i++) {

				Linkable linkable = (Linkable) node.getProtocol(FastConfig
						.getLinkable(pid));
				if (linkable.degree() > 0) {
					Node peern = linkable.getNeighbor(r.nextInt(linkable
							.degree()));

					// System.out.println("streamer sending to " +
					// peern.getIndex());

					// XXX quick and dirty handling of failures
					// (message would be lost anyway, we save time)
					if (!peern.isUp())
						return;
					((Transport) node.getProtocol(FastConfig.getTransport(pid)))
							.send(node, peern, new Info(info, (Usernode) node,
									0), pid);
				}

			}

			info++;
		}

	}

	public void processEvent(Node node, int pid, Object event) {
	}
}
