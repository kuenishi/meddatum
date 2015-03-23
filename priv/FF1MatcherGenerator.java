import java.io.File;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class FF1MatcherGenerator {
	public static void toFunction(List<String> list){
		String fLine = list.get(0);
		String arr[] = fLine.split("\t");
		String code = arr[0];
		String fields[] = new String[9];
		for(String l : list){
			arr = l.split("\t");
			int num = Integer.parseInt(arr[2]);
			String v = arr[6];
			fields[num - 1] = v;
		}
		System.out.printf("to_list(\"%s\", [" , code);
		boolean first = true;
		for(String f : fields){
			if(!first){
				System.out.print(",");
			}else{
				first = false;
			}
			if(f == null){
				System.out.print(" _ ");
			}else{
				char fchar = f.charAt(0);
				String fup = Character.toUpperCase(fchar) + f.substring(1);
				System.out.print(" " + fup);
			}
		}
		System.out.println("]) ->");
		System.out.print("    [");
		first = true;
		for(String f : fields){
			if(f == null){
				continue;
			}
			if(!first){
				System.out.print(",");
			}else{
				first = false;
			}
			char fchar = f.charAt(0);
			String fup = Character.toUpperCase(fchar) + f.substring(1);
			System.out.printf("{%s, %s}", f , fup);
		}
		System.out.println("];");
	}
	public static void main(String[] args) throws Exception{
		Scanner sc = new Scanner(new File("tmp.txt"));
		List<String> list = new ArrayList<String>();
		while(sc.hasNextLine()){
			String line = sc.nextLine();
			String arr[] = line.split("\t");
			if(!arr[0].isEmpty() && !list.isEmpty()){
				toFunction(list);
				list.clear();
			}
			list.add(line);
		}
		toFunction(list);
		sc.close();
	}
}
