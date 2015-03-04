package hyparview;


import java.util.ArrayList;

import peersim.cdsim.CDProtocol;
import peersim.config.Configuration;
import peersim.core.CommonState;
import peersim.core.Linkable;
import peersim.core.Network;
import peersim.core.Node;
import peersim.util.IndexIterator;
import peersim.util.RandPermutation;


public class HyParViewJoinTest implements Linkable, CDProtocol{

	/***************************************
	 * Parameters for configuration
	 ***************************************/
	public final static String PAR_ACTIVE_VIEW_SIZE = "AViewSize";
	public final static String PAR_PASSIVE_VIEW_SIZE = "PViewSize";
	public final static String PAR_PASSIVE_SHUFFLE_LENGHT = "PassiveShuffleL";
	public final static String PAR_ACTIVE_SHUFFLE_LENGHT = "ActiveShuffleL";
	public final static String PAR_PASSIVE_RANDOM_LENGHT = "PassiveRandomL";
	public final static String PAR_RANDOM_LENGHT = "RandomL";

	// This will control the overlay that is tested in simulation
	public final static String PAR_MONITORIZATION = "monitor";
	// This are the possibilities to use in the monitor parameter
	public final static String MON_ACTIVE = "active";
	public final static String MON_PASSIVE = "passive";
	// This are the values to the variable that controls this process
	public final static int MON_ACTIVE_VAL = 0;
	public final static int MON_PASSIVE_VAL = 1;

	/***************************************
	 * Vars for parameters configuration
	 ***************************************/
	private static String name;
	public static int protocolID;

	private static int activeViewSize = 20;
	private static int passiveViewSize = 30;
	private static int randomLenght = 6;
	private static int passiveRandomLenght = 3;

	// For selecting witch overlay to monitor
	private int overlayMonitor = MON_ACTIVE_VAL;

	/***************************************
	 * Internal State of the node protocol
	 ***************************************/
	private Node[] activeMembership;
	private Node[] passiveMembership; // Update 24-10-2006: Changed from
										// AgeableNode to Node

	private int activeDegree = 0;
	private int passiveDegree = 0;

	/*******************************************************
	 * Vars for suporting the MembershipProtocol Interface
	 *******************************************************/
	protected Node myNode;
	protected int msgSent;

	/*******************************************************
	 * Other vars
	 *******************************************************/
	private IndexIterator peerIterator;
	ArrayList<Node> sentList;

	/*******************************************************
	 * Join Statistics
	 *******************************************************/

	/**
	 * Constructor
	 * 
	 * @param name
	 */
	public HyParViewJoinTest(String name) {
		// Read configuration
//		HyParViewJoinTest.name = name;
		HyParViewJoinTest.protocolID = Configuration.lookupPid(name
				.replaceFirst("protocol.", ""));
//		HyParViewJoinTest.activeViewSize = Configuration
//				.getInt(HyParViewJoinTest.name + "." + PAR_ACTIVE_VIEW_SIZE);
//		HyParViewJoinTest.passiveViewSize = Configuration
//				.getInt(HyParViewJoinTest.name + "." + PAR_PASSIVE_VIEW_SIZE);
//		HyParViewJoinTest.randomLenght = Configuration
//				.getInt(HyParViewJoinTest.name + "." + PAR_RANDOM_LENGHT);
//		HyParViewJoinTest.passiveRandomLenght = Configuration
//				.getInt(HyParViewJoinTest.name + "."
//						+ PAR_PASSIVE_RANDOM_LENGHT);
//
//		String monitorizationTarget = Configuration
//				.getString(HyParViewJoinTest.name + "." + PAR_MONITORIZATION);
//		if (monitorizationTarget.compareTo(MON_ACTIVE) == 0)
//			this.overlayMonitor = MON_ACTIVE_VAL;
//		else if (monitorizationTarget.compareTo(MON_PASSIVE) == 0)
//			this.overlayMonitor = MON_PASSIVE_VAL;
//		else {
//			System.err
//					.println("Invalid value for the Monitorization parameter.");
//			System.exit(1);
//		}

		// Init protocol configuration
		this.activeMembership = new Node[HyParViewJoinTest.activeViewSize];
		for (int i = 0; i < HyParViewJoinTest.activeViewSize; i++)
			this.activeMembership[i] = null;

		this.passiveMembership = new Node[HyParViewJoinTest.passiveViewSize];
		for (int i = 0; i < HyParViewJoinTest.passiveViewSize; i++)
			this.passiveMembership[i] = null;

		this.activeDegree = 0;
		this.passiveDegree = 0;

		// Start other protocol information with default values
		this.myNode = null;
		this.msgSent = 0;
		this.peerIterator = new RandPermutation(CommonState.r);
		this.sentList = null;

	}

	/****************************************
	 * Protocol Specific Methods
	 ****************************************/

	/**
	 * This is used to generate fixed lenght random walks in the overlay when
	 * adding nodes this assumes no null in between nodes at active view
	 */
	public boolean randomWalk(Node newMember, int ttl, Node originator,
			int protocolID) {
		boolean status = this.myNode.isUp(); // check if node is up

		if (status) {
			// This node is up -> he can (and will) execute protocol
			if (ttl <= 0 || this.activeDegree <= 1) {
				// We first check if we can add the new neighbour to the view
				// and only if we can we do it
				if (this.canAddNeighbour2ActiveMembership(newMember)) {
					// we can and shoud try to connect to new member
					if (((HyParViewJoinTest) newMember
							.getProtocol(HyParViewJoinTest.protocolID))
							.joinConnect(this.myNode)) {

						// If active view is full make a disconect from someone
						// random
						if (this.activeDegree == HyParViewJoinTest.activeViewSize) {
							this.activeRemove();
							this.compressArray(this.activeMembership);
						}

						// Add node to ones current view
						if (!this.activeMembershipAddNeighbour(newMember)) {
							// TODO: is this the better aproach??? Think about
							// this with time
						//	System.err
							//		.println("Warning: A random walked stoped here but i could not add the new node: random walk droped!");
						} else {
//							this.jstat.newNeighbor();
						}
					} else {
						// This shoud not happen
						//System.err
							//	.println("Warning: A peer i received in a random walk refused my connect atempt");
					}
				}
			} else {
				// Check if this is the middle point of the random walk: if so
				// add this node to the passive membership
				if (ttl == HyParViewJoinTest.passiveRandomLenght) {
					if (this.canAddNeighbour2PassiveMembership(newMember)) {
						this.passiveMembershipAddNeighbour(newMember);
					}
				}
				// Select a valid target to forward this request
				Node destino = null;
				this.peerIterator.reset(this.activeDegree);
				while (destino == null || destino.equals(originator))
					destino = this.activeMembership[this.peerIterator.next()];

				((HyParViewJoinTest) destino.getProtocol(protocolID))
						.randomWalk(newMember, ttl - 1, this.myNode, protocolID);
			}
		}

		return status;
	}

	private void filterList(ArrayList<Node> peerList) {
		for (int i = 0; i < peerList.size(); i++) {
			if (this.myNode.equals(peerList.get(i))
					|| this.activeContains(peerList.get(i))
					|| this.passiveContains(peerList.get(i))) {
				peerList.remove(i);
				i--;
			}
		}
	}

	private void mergeView(ArrayList<Node> myList, ArrayList<Node> peerList) {
		// Preparation: Crete a copy (local) of my passiveView
		ArrayList<Node> passiveMembershipLocalCopy = new ArrayList<Node>();
		for (int i = 0; i < HyParViewJoinTest.passiveViewSize; i++)
			if (this.passiveMembership[i] != null)
				passiveMembershipLocalCopy.add(this.passiveMembership[i]);

		// REMOVE NEXT LINE
		// System.out.println("Node " + this.myNode.getID() +
		// ": size of my list before clean: " + myList.size());

		// Clean myList of elements not present in my passive view (myself or
		// members of my active view)
		for (int i = 0; i < myList.size(); i++)
			if (!this.passiveContains(myList.get(i))) {
				myList.remove(i);
				i--;
			}
		// RENOVE NEXT LINE
		// System.out.println("Node " + this.myNode.getID() +
		// ": size of my list after clean: " + myList.size());

		while (this.passiveDegree < HyParViewJoinTest.passiveViewSize
				&& peerList.size() > 0) {
			this.passiveMembershipAddNeighbour(peerList.remove(0));
		}

		Node toRemove = null;
		while (peerList.size() > 0) {
			if (myList.size() > 0) {
				toRemove = myList.remove(0);
				this.passiveRemove(toRemove);
				passiveMembershipLocalCopy.remove(toRemove);
			} else {
				// Select a random element that was present in the passive view
				// at the start of this operation
				this.peerIterator.reset(passiveMembershipLocalCopy.size());
				this.passiveRemove(passiveMembershipLocalCopy
						.remove(this.peerIterator.next()));
			}
			this.passiveMembershipAddNeighbour(peerList.remove(0));
		}
	}

	/**
	 * This is to be called by a peer that wishes to add this node to his active
	 * view this should be called when this operation is done in the context of
	 * a Join Request (A node is joining to the overlay)
	 */
	public Boolean joinConnect(Node peer) {
		Boolean status = null;

		if (this.myNode.isUp()) {
			if (this.activeDegree == HyParViewJoinTest.activeViewSize) {
				// Drop someone so that we can add this new node
				this.activeRemove();
	//			this.jstat.randomRemoved();
				this.compressArray(this.activeMembership);
			}

			status = new Boolean(this.activeMembershipAddNeighbour(peer));
			if (status.booleanValue()){}
		//		this.jstat.newNeighbor();
		}

		return status;
	}

	/**
	 * This is to be called by a peer that wishes to add this node to his active
	 * view this should be called when this operation is done in the context of
	 * a Join Request (A node is joining to the overlay)
	 */
	public Boolean neighbourConnect(Node peer) {
		Boolean reply = null;

		if (this.myNode.isUp()) {
			if (this.activeDegree < HyParViewJoinTest.activeViewSize) {
				reply = new Boolean(this.activeMembershipAddNeighbour(peer));
			} else {
				reply = new Boolean(false);
			}
		}

		return reply;
	}

	/**
	 * 
	 */
	public boolean shuffleReply(ArrayList<Node> peerReply) {
		boolean reply = this.myNode.isUp();

		if (reply) {
			this.filterList(peerReply);
			this.mergeView(this.sentList, peerReply);
		}

		return reply;
	}

	/**
	 * This is to be called by a peer that wishes to close a conection in the
	 * active membership
	 */
	public void disconnect(Node peer) {
		int position = -1;

		for (int i = 0; position == -1 && i < HyParViewJoinTest.activeViewSize
				&& i < activeMembership.length; i++)
			if (this.activeMembership[i] != null
					&& this.activeMembership[i].equals(peer))
				position = i;

		if (position != -1) {
			Node temp = this.activeMembership[position];
			this.activeMembership[position] = null;
			this.activeDegree--;
			this.compressArray(this.activeMembership);
			this.passiveMembershipAddNeighbour(temp); // 22-novembro-2006
			//this.jstat.disconnected();
		} else {
		//	System.err
			//		.println("Warning: Received a disconnect notification from someone i am not neighbour");
		}
	}

	/**
	 * This function validates if a given node can be added to the current
	 * active membership
	 */
	private boolean canAddNeighbour2ActiveMembership(Node neighbour) {
		return !this.activeContains(neighbour)
				&& !this.myNode.equals(neighbour);
	}

	/**
	 * This function validates if a given node can be added to the current
	 * passive memberhip
	 */
	private boolean canAddNeighbour2PassiveMembership(Node neighbour) {
		return !this.activeContains(neighbour)
				&& !this.passiveContains(neighbour)
				&& !this.myNode.equals(neighbour);
	}

	/**
	 * Add's this node to the active membership checking the associated
	 * constraints
	 */
	private boolean activeMembershipAddNeighbour(Node neighbour) {
		boolean sucess = false;
		// Updated this code to use the new verify function to check if the node
		// can be added
		// if(!(this.activeContains(neighbour) || this.activeDegree >=
		// HybridMembership.activeViewSize))
		if (this.canAddNeighbour2ActiveMembership(neighbour)
				&& this.activeDegree < HyParViewJoinTest.activeViewSize)
			for (int i = 0; !sucess && i < HyParViewJoinTest.activeViewSize; i++) {
				if (this.activeMembership[i] == null) {
					this.activeMembership[i] = neighbour;
					this.activeDegree++;

					// Garantee that the node is not in the passive membership
					this.passiveRemove(neighbour);

					sucess = true;
				}
			}

		return sucess;
	}

	/**
	 * Add's this node to the passive membership checking the associated
	 * constraints
	 */
	private boolean passiveMembershipAddNeighbour(Node neighbour) {
		boolean sucess = false;
		if (this.canAddNeighbour2PassiveMembership(neighbour)) {
			if (this.passiveDegree < HyParViewJoinTest.passiveViewSize) {
				// Insert member in a free slot
				int i = 0;
				while (this.passiveMembership[i] != null)
					i++;
				this.passiveMembership[i] = neighbour;
				this.passiveDegree++;
			} else {
				// Select random member to drop
				this.peerIterator.reset(HyParViewJoinTest.passiveViewSize);
				this.passiveMembership[this.peerIterator.next()] = neighbour;
				//System.err
					//	.println("Warning: random member droped in passive view");
			}
			sucess = true;
		}

		return sucess;
	}

	/**
	 * Check's if a node is present in the active membership of this node
	 */
	private boolean activeContains(Node neighbour) {
		boolean found = false;

		for (int i = 0; !found && i < HyParViewJoinTest.activeViewSize; i++)
			found = this.activeMembership[i] != null
					&& this.activeMembership[i].equals(neighbour);

		return found;
	}

	/**
	 * Check's if a node is present in the passive membership of this node
	 */
	private boolean passiveContains(Node neighbour) {
		boolean found = false;

		for (int i = 0; !found && i < HyParViewJoinTest.passiveViewSize; i++)
			found = this.passiveMembership[i] != null
					&& this.passiveMembership[i].equals(neighbour);

		return found;
	}

	/**
	 * Remove a random node from the active membership it notifies the node with
	 * a disconect msg
	 * 
	 * @return
	 */
	private boolean activeRemove() {
		this.peerIterator.reset(this.activeDegree);
		int target = this.peerIterator.next();
		this.compressArray(this.activeMembership);
		((HyParViewJoinTest) this.activeMembership[target]
				.getProtocol(HyParViewJoinTest.protocolID))
				.disconnect(this.myNode);
		this.msgSent++; // Increase msg sent because of the disconect msg

		// Remeber who i removed...
		Node removed = this.activeMembership[target];
		// Remove this node from the active view
		this.activeMembership[target] = null;
		this.compressArray(this.activeMembership);
		this.activeDegree--;

		// Pass this node to my passive view
		if (this.canAddNeighbour2PassiveMembership(removed)) {
			this.passiveMembershipAddNeighbour(removed);
		}

		return true;
	}

	/**
	 * Remove the given node from the passive view of this node if it's presente
	 */
	private boolean passiveRemove(Node neighbour) {
		boolean found = false;
		int i = 0;

		for (i = 0; !found && i < HyParViewJoinTest.passiveViewSize; i++) {
			found = this.passiveMembership[i] != null
					&& this.passiveMembership[i].equals(neighbour);
		}

		// The for cycle will increment one extra unit during the final test of
		// the found variable
		if (found) {
			this.passiveMembership[i - 1] = null;
			this.passiveDegree--;
			this.compressArray(this.passiveMembership);
		}

		return found;
	}

	/****************************************
	 * Generic Java Methods
	 ****************************************/

	public Object clone() {
		HyParViewJoinTest nhm = null;
		try {
			nhm = (HyParViewJoinTest) super.clone();
			// Set the vars of the new instance, doing a deep cloning...
			nhm.activeMembership = new Node[HyParViewJoinTest.activeViewSize];
			for (int i = 0; i < HyParViewJoinTest.activeViewSize; i++)
				nhm.activeMembership[i] = null;
			nhm.passiveMembership = new Node[HyParViewJoinTest.passiveViewSize];
			for (int i = 0; i < HyParViewJoinTest.passiveViewSize; i++)
				nhm.passiveMembership[i] = null;
			nhm.activeDegree = 0;
			nhm.passiveDegree = 0;
			nhm.myNode = null;
			nhm.msgSent = 0;
			nhm.sentList = null;
			nhm.peerIterator = new RandPermutation(CommonState.r); // Change
																	// Log:
																	// 13-11-2006:Jleitao
																	// Notei que
																	// falhava
																	// esta
																	// coisa...

			// Join Observation
		//	nhm.jstat = new JoinStatistics();
		} catch (CloneNotSupportedException e) {
			// never happens
		}
		return nhm;
	}

	/****************************************
	 * Interface CDProtocol
	 ****************************************/

	public void nextCycle(Node node, int protocolID) {
		// This is used to do the clean up for each round.
		/*System.out.println("Node of cycle: " + node.getID() + " -> ");
		System.out.println("Node: " + this.myNode.getID() + " -> ");

		for (Node nodee : activeMembership) {
			if (nodee != null) {
				System.out.println(nodee.getID() + " ");
			}
		}
		System.out.println();*/
	//	this.jstat.reset();
	}

	/****************************************
	 * Interface Linkable
	 ****************************************/

	public int degree() {
		if (this.overlayMonitor == MON_ACTIVE_VAL)
			return this.activeDegree;
		else
			return this.passiveDegree;
	}

	public Node getNeighbor(int i) {
		if (this.overlayMonitor == MON_ACTIVE_VAL)
			return this.activeMembership[i];
		else {
			// for(int p = 0; p < this.passiveMembership.length; p++)
			// System.out.print((this.passiveMembership[p] == null ? "null, " :
			// this.passiveMembership[p].getID()+", "));
			// System.out.println();
			return this.passiveMembership[i];
		} // return (this.passiveMembership[i] == null ? null :
			// this.passiveMembership[i].getNode());
	}

	public boolean addNeighbor(Node neighbour) {
		if (this.overlayMonitor == MON_ACTIVE_VAL)
			return this.activeMembershipAddNeighbour(neighbour);
		else
			return this.passiveMembershipAddNeighbour(neighbour);
	}

	public boolean contains(Node neighbor) {
		if (this.overlayMonitor == MON_ACTIVE_VAL)
			return this.activeContains(neighbor);
		else
			return this.passiveContains(neighbor);
	}

	public void pack() {
		// nothing to do...
	}

	public void onKill() {
		this.activeMembership = new Node[0];
		this.passiveMembership = new Node[0];
		this.activeDegree = 0;
		this.passiveDegree = 0;
	}

	/****************************************
	 * MembershipProtocol
	 ****************************************/

	/**
	 * @Deprecated
	 */
	public void setMyId(int myID) {
		return;
	}

	public Object shuffle(Object target, int protocolID, int round) {
		return null;
	}

	/**
	 * join method. This method is invoked by a controller that simulates the
	 * initialization of the overlay. The simulator will pass to the contact
	 * node (a node already present in the overlay) the information of the
	 * joining node. In this method the algorithm to be executed by the contact
	 * node should be implemented.
	 * 
	 * One interesting concept of the Hybrid Membership Protocol is that we must
	 * run to join protocols One for the active overlay and other in the passive
	 * overlay
	 */
	public void join(Node newMember, int protocolID) {
		int position = 0;

		// Number of forwarded joins should be the minimum of this two values
		int tosend = (this.activeDegree <= (HyParViewJoinTest.activeViewSize - 1) ? this.activeDegree
				: (HyParViewJoinTest.activeViewSize - 1));

		/**
		 * TODO: Make the length of the random walks change :D and see what
		 * happens Note: this piece of code assumes that there are not free
		 * (null) spaces in the active membership array
		 */

		// Because our local view can change (e.g. some neighbor drops a
		// connection to us)
		ArrayList<Node> peersSelected = new ArrayList<Node>();

		this.peerIterator.reset(this.activeDegree);

		for (int i = 0; i < tosend; i++)
			peersSelected.add(this.activeMembership[this.peerIterator.next()]);

		while (peersSelected.size() > 0) {
			((HyParViewJoinTest) peersSelected.remove(0)
					.getProtocol(protocolID)).randomWalk(newMember,
					HyParViewJoinTest.randomLenght, myNode, protocolID);
			this.msgSent++; // Increase the number of messages sent by this node
		}

		// Now try to connect to new member
		if (((HyParViewJoinTest) newMember
				.getProtocol(HyParViewJoinTest.protocolID)).joinConnect(
				this.myNode).booleanValue()) {

			if (this.activeDegree == HyParViewJoinTest.activeViewSize) {
				// We have to replace a member, and notify him to establish a
				// new conection to new member :D
				peerIterator.reset(HyParViewJoinTest.activeViewSize);
				position = peerIterator.next();
				((HyParViewJoinTest) this.activeMembership[position]
						.getProtocol(HyParViewJoinTest.protocolID))
						.disconnect(this.myNode);
			//	this.jstat.randomRemoved();
				this.msgSent++; // number of msgs rise cause of disconnect msg
								// (maybe this should not happen)
			} else {
				while (this.activeMembership[position] != null)
					position++;
				this.activeDegree++;
			}

		//	this.jstat.newNeighbor();
			this.activeMembership[position] = newMember;
		} else {
			//System.err.println("Warning: Connection to new member failed");
		}
	}

	public Node[] neighbors() {
		Node[] answer;
		int i = 0, j = 0;

		if (this.overlayMonitor == MON_ACTIVE_VAL) {
			answer = new Node[this.activeDegree];
			while (i < this.activeDegree) {
				while (this.activeMembership[j] == null)
					j++;
				answer[i] = this.activeMembership[j];
				i++;
				j++;
			}
		} else {
			answer = new Node[this.passiveDegree];
			while (i < this.passiveDegree) {
				while (this.passiveMembership[j] == null)
					j++;
				answer[i] = this.passiveMembership[j];
				i++;
				j++;
			}
		}

		return answer;
	}

	public int getViewSize() {
		if (this.overlayMonitor == MON_ACTIVE_VAL)
			return HyParViewJoinTest.activeViewSize;
		else
			return HyParViewJoinTest.passiveViewSize;
	}

	public void setMyNode(Node me) {
		this.myNode = me;
	}

	public int getSentMsg() {
		return this.msgSent;
	}

	public Node getGossipTarget(int i) {
		return this.getNeighbor(i);
	}

	/********************************************
	 * Failable methods
	 ********************************************/

	public void fail() {
		this.activeMembership = new Node[HyParViewJoinTest.activeViewSize];
		this.passiveMembership = new Node[HyParViewJoinTest.passiveViewSize];
		for (int i = 0; i < HyParViewJoinTest.activeViewSize; i++)
			this.activeMembership[i] = null;
		for (int i = 0; i < HyParViewJoinTest.passiveViewSize; i++)
			this.passiveMembership[i] = null;
		this.activeDegree = 0;
		this.passiveDegree = 0;

		Network.remove(this.myNode.getIndex());
	}

	/********************************************
	 * Auxiliary methods
	 ********************************************/

	/**
	 * This method should clear a array of all null positions, in a way that all
	 * not null entrys should be in the start of the array, the logical order of
	 * the nodes is mantained
	 */
	protected void compressArray(Object[] array) {
		Object[] temp = new Object[array.length];
		int position = 0;

		for (int i = 0; i < array.length; i++) {
			if (array[i] != null) {
				temp[position] = array[i];
				position++;
			}
		}

		for (; position < temp.length; position++)
			temp[position] = null;

		System.arraycopy(temp, 0, array, 0, array.length);
	}

	public void setMonitorActive() {
		this.overlayMonitor = MON_ACTIVE_VAL;
	}

	public void setMonitorPassive() {
		this.overlayMonitor = MON_PASSIVE_VAL;
	}


	// Class end's here!
}
