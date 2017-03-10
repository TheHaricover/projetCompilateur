import java.util.Scanner;

public class Wrapper {

    public static void main(String[] args){

        int i;
        i = MyClass.even(Integer.parseInt(args[0]),Integer.parseInt(args[1]));
        System.out.println("result: "+ i);


    }

}
