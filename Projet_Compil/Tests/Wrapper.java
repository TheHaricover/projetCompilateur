import java.util.Scanner;

public class Wrapper {

    public static void main(String[] args){

        int i;
        if (args.length >= 2){
          i = MyClass.even(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
          System.out.println("result: " + i);
        }else{
          i = MyClass.even(Integer.parseInt(args[0]), 0);
          System.out.println("result: " + i);
        }



    }

}
