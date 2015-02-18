package example.basicGossip;

import java.util.ArrayList;
import java.util.Collections;

import peersim.cdsim.CDProtocol;
import peersim.config.Configuration;
import peersim.config.FastConfig;
import peersim.core.CommonState;
import peersim.core.Linkable;
import peersim.core.Node;
import peersim.edsim.EDProtocol;
import peersim.transport.Transport;
import peersim.vector.SingleValueHolder;

/**
 * Event driven version of epidemic averaging.
 */
public class BasicGossip extends SingleValueHolder implements CDProtocol,
		EDProtocol {

	protected int fanout;
	protected int networkSize;
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
		fanout = (int) Math.log(networkSize);
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
		System.out.println("PID " + pid);


		// streamer
		if (node.getIndex() == 0) {
			//System.out.println("streamer generating " + info);
			for (int i = 0; i < fanout; i++) {
				Linkable linkable = (Linkable) node.getProtocol(FastConfig
						.getLinkable(pid));
				if (linkable.degree() > 0) {
					Node peern = linkable.getNeighbor(CommonState.r
							.nextInt(linkable.degree()));

					//System.out.println("streamer sending to " + peern.getIndex());
					
					// XXX quick and dirty handling of failures
					// (message would be lost anyway, we save time)
					if (!peern.isUp())
						return;

					((Transport) node.getProtocol(FastConfig.getTransport(pid)))
							.send(node, peern, new AverageMessage(info, node),
									pid);
				}

			}

			info++;
		}

	}

	// --------------------------------------------------------------------------

	/**
	 * This is the standard method to define to process incoming messages.
	 */
	public void processEvent(Node node, int pid, Object event) {

		AverageMessage aem = (AverageMessage) event;
		//System.out.println("Node: " + node.getIndex() + " received message " + aem.value);

		if (!((Usernode) node).containsElem(aem.value)) {
			// first time we receive this message
			((Usernode) node).saveMessage(aem.value);
			for (int i = 0; i < fanout; i++) {
				Linkable linkable = (Linkable) node.getProtocol(FastConfig
						.getLinkable(pid));
				if (linkable.degree() > 0) {
					Node peern = linkable.getNeighbor(CommonState.r
							.nextInt(linkable.degree()));
	
					// XXX quick and dirty handling of failures
					// (message would be lost anyway, we save time)
					if (!peern.isUp())
						return;

					((Transport) node.getProtocol(FastConfig.getTransport(pid)))
							.send(node, peern, new AverageMessage(info, node),
									pid);
				}

			}
			//System.out.println("Node: " + node.getIndex() );
			//((Usernode) node).dumpMessageList();
			
		} 

		/*
		 * if (aem.sender != null) ((Transport)
		 * node.getProtocol(FastConfig.getTransport(pid))).send( node,
		 * aem.sender, new AverageMessage(value, null), pid);
		 * 
		 * value = (value + aem.value) / 2;
		 */
	}

	private ArrayList<Integer> getRandomIds(int size) {
		ArrayList<Integer> list = new ArrayList<Integer>();
		for (int i = 0; i < size; i++) {
			// list.add(new Random().nextInt(network));
		}
		Collections.shuffle(list);
		return list;
	}
}

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

/**
 * The type of a message. It contains a value of type double and the sender node
 * of type {@link peersim.core.Node}.
 */
class AverageMessage {

	final int value;
	/**
	 * If not null, this has to be answered, otherwise this is the answer.
	 */
	final Node sender;

	public AverageMessage(int value, Node sender) {
		this.value = value;
		this.sender = sender;
	}
}
