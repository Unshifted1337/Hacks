package fragment.submissions;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

public class JonathanRead {

	public static void main(String[] args) {
		try (BufferedReader in = new BufferedReader(new FileReader(args[0]))) {
			String fragmentProblem;
			while ((fragmentProblem = in.readLine()) != null) {
				System.out.println(reassemble(fragmentProblem));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static String reassemble(String input) throws InterruptedException {
		HashMap<Integer, FragmentMerge> overlappingFragments;
		String output = "";

		ArrayList<String> fragments = new ArrayList<String>(Arrays.asList(input.split(";")));

		// while more than one fragment exists
		while (fragments.size() > 1) {
			// get all overlapping fragments
			overlappingFragments = getOverlappingFragments(fragments);

			// find the best one on the line
			FragmentMerge merge = getBiggestOverlap(overlappingFragments);

			// merge it

			mergeFragment(fragments, merge);
		}

		output = fragments.get(0);
		return output;
	}

	private static void mergeFragment(ArrayList<String> fragments, FragmentMerge merge) {
		String startingFragment = fragments.get(merge.getStartingFragmentIndex());
		String endingFragment = fragments.get(merge.getEndingFragmentIndex());
		int overlapIndex = merge.getOverlapIndex();

		// merge the fragments
		String mergedFragments = startingFragment.substring(0, overlapIndex) + endingFragment;

		// overwrite the first
		fragments.set(merge.getStartingFragmentIndex(), mergedFragments);

		// remove the second
		fragments.remove(merge.getEndingFragmentIndex());
	}

	private static FragmentMerge getBiggestOverlap(HashMap<Integer, FragmentMerge> merges) {
		int maxOverlapAmount = 0;
		int maxOverlapKey = 0;
		int currentOverlapAmount = 0;

		// check there is something to merge
		if (merges == null || merges.size() == 0) {
			System.out.println("Nothing to merge");
			return null;
		}

		// for each match that was stored
		for (Integer key : merges.keySet()) {
			// get the overlap amount
			currentOverlapAmount = merges.get(key).getOverlapAmount();

			// if it's the best match on the line (so far) then store the key
			if (currentOverlapAmount > maxOverlapAmount) {
				maxOverlapAmount = currentOverlapAmount;
				maxOverlapKey = key;
			}
		}

		return merges.get(maxOverlapKey);
	}

	private static HashMap<Integer, FragmentMerge> getOverlappingFragments(ArrayList<String> fragments) {
		HashMap<Integer, FragmentMerge> overlappingFragments = new HashMap<Integer, FragmentMerge>();

		// for each fragment in the line
		for (int endingFragment = 0; endingFragment < fragments.size(); endingFragment++) {
			// for each other fragment in the line
			for (int startingFragment = 0; startingFragment < fragments.size(); startingFragment++) {
				// ensure not comparing itself
				if (endingFragment != startingFragment) {
					// see if there is an overlap
					int overlapIndex = getOverlapIndex(fragments, startingFragment, endingFragment);

					// if the fragment was removed, skip to the next one
					if (overlapIndex == -1) {
						break;
					} else if (overlapIndex > 0) {
						// otherwise check if this fragment already has a match
						FragmentMerge existingMerge = overlappingFragments.get(startingFragment);

						// if it doesn't, or this one is better, then store the merge
						if (existingMerge == null || overlapIndex > existingMerge.getOverlapAmount()) {
							int overlapAmount = fragments.get(startingFragment).length() - overlapIndex;

							FragmentMerge merge = new FragmentMerge(startingFragment, endingFragment, overlapIndex, overlapAmount);
							overlappingFragments.put(startingFragment, merge);
						}
					}
				}
			}
		}

		return overlappingFragments;
	}

	private static int getOverlapIndex(ArrayList<String> fragments, int startingFragment, int endingFragment) {
		int overlapIndex = 0;
		boolean isOverlap = false;
		int matchLetterIndex = 0;

		// for each letter in the second fragment
		for (int checkLetterIndex = 0; checkLetterIndex < fragments.get(startingFragment).length(); checkLetterIndex++) {
			char ch1 = fragments.get(endingFragment).charAt(matchLetterIndex);
			char ch2 = fragments.get(startingFragment).charAt(checkLetterIndex);

			// if the letters match
			if (ch1 == ch2) { // match
				// if this is the first match, set overlap
				if (!isOverlap) { // is first match?
					overlapIndex = checkLetterIndex;
					isOverlap = true;
				} else if (matchLetterIndex == fragments.get(endingFragment).length() - 1) {
					// match in middle of fragment, abort!
					fragments.remove(endingFragment);
					return -1;
				} else if (checkLetterIndex == fragments.get(startingFragment).length() - 1) {
					// last match so full match found
					return overlapIndex;
				}
				matchLetterIndex++;
			} else if (isOverlap) {
				// letters don't match - reset overlap
				overlapIndex = 0;
				isOverlap = false;
				matchLetterIndex = 0;
			}
		}
		return 0;
	}

	static class FragmentMerge {

		private int	mStartingFragmentIndex;
		private int	mEndingFragmentIndex;
		private int	mOverlapIndex;
		private int	mOverlapAmount;

		FragmentMerge(int startingFragment, int endingFragment, int overlapIndex, int overlapAmount) {
			mStartingFragmentIndex = startingFragment;
			mEndingFragmentIndex = endingFragment;
			mOverlapIndex = overlapIndex;
			mOverlapAmount = overlapAmount;
		}

		public int getStartingFragmentIndex() {
			return mStartingFragmentIndex;
		}

		public int getEndingFragmentIndex() {
			return mEndingFragmentIndex;
		}

		public int getOverlapIndex() {
			return mOverlapIndex;
		}

		public int getOverlapAmount() {
			return mOverlapAmount;
		}

	}
}
