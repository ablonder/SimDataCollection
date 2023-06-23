package model;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import sim.field.network.Edge;
import sim.field.network.Network;
import sim.util.Bag;

public class NetworkLoader {
	/*
	 * An additional utility for creating a network object from a file
	 */
	public Network readNetwork(String fname, HashMap<String, Object> nodes, String sep) {
		// create a new empty network
		Network net = new Network();
		// now I can try to open the file
		try {
			BufferedReader file = new BufferedReader(new FileReader(fname));
			// store an array of indices for which column to look at for the relevant values default to the first 3
			int[] cols = new int[] {0, 1, 2};
			// loop through the file
			String line;
			while((line = file.readLine()) != null) {
				// skip empty lines or those that begin with the comment character (%)
				if(line.length() < 1 || line.charAt(0) == '%') continue;
				// now split the line by the separator (and convert it to lowercase for convenience)
				String[] splitline = line.toLowerCase().split(sep);
				// make sure there are at least 2 things in the line
				if(splitline.length < 2) continue;
				// check if it is a header (contains "To" and "From")
				List<String> splitlist = Arrays.asList(splitline);
				if(splitlist.contains("to") && splitlist.contains("from")) {
					// if so, reload the column indicies into the array accordingly
					cols = new int[] {splitlist.indexOf("from"), splitlist.indexOf("to"), splitlist.indexOf("info")};
					// and then go to the next row (since this isn't a real row of data)
					continue;
				}
				// create a list to hold the current edge
				Object[] edge = new Object[2];
				// the to and from will be handled basically the same
				for(int i = 0; i < 2; i++) {
					// grab the name of this node
					String s = splitline[cols[i]].strip();
					// then grab the node itself from the hash map
					Object n = nodes.get(s);
					// if it doesn't really exist, create it
					if(n == null) {
						n = new Node(s);
						// and add it to the map
						nodes.put(s, n);
					}
					// and then add it to the current edge
					edge[i] = n;
				}
				// now check if there's any edge info provided and add the edge to the network accordingly
				if(splitline.length > 2 && cols[2] > -1) {
					net.addEdge(edge[0], edge[1], splitline[cols[2]].strip());
				} else {
					net.addEdge(edge[0], edge[1], "");
				}
			}
			// now that the whole file has been loaded in, append the ArrayList onto the end of the provided array
			
		} catch(IOException e) {
			System.out.println("Invalid network file!");
			System.exit(0);
		}
		return net;
	}
	
	/*
	 * Alias for read network without a hash map (all nodes will be created as they're read in)
	 */
	public Network readNetwork(String fname, String sep) {
		return readNetwork(fname, new HashMap<String, Object>(), sep);
	}
	
	/*
	 * Alias for read network that handles converting a Bag of nodes to a hash map
	 */
	public Network readNetwork(String fname, Bag nodes, String sep) {
		HashMap<String, Object> n = new HashMap<String, Object>();
		for(int i = 0; i < nodes.numObjs; i++) {
			if(nodes.objs[i] != null) {
				n.put(""+i, nodes.objs[i]);
			}
		}
		return readNetwork(fname, n, sep);
	}
	
	/*
	 * Other aliases for read network that handle other types of list as input (by converting them to a Bag)
	 */
	public Network readNetwork(String fname, Object[] nodes, String sep) {
		return readNetwork(fname, new Bag(nodes), sep);
	}
	
	public Network readNetwork(String fname, List<Object> nodes, String sep) {
		return readNetwork(fname, new Bag(nodes), sep);
	}
	
	/*
	 * Helper class to point to the actual node object - users are encouraged to store a pointer to this container
	 */
	public class Node {
		public Object obj;
		public String name;
		
		public Node(String s) {
			this.name = s;
		}
	}
}
