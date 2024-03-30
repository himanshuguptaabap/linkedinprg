"The simple idea behind this OOPs program is to give you
"an idea how you can create an abstract class.
"I have also written points which are worth remembering.
"Working with abstract class is also a bonus since you
"know how you can work with them in ABAP RTTS.
REPORT zrg_abstract.
CLASS lcl_abstract DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS : a_m1,
      a_m2,
      a_m3 ABSTRACT.
    "Since static methods can't be redefined so they can't be abstract.
    "Also, abstract methods can't be in private section since they can't
    "inherited or redefined.
    "Final methods can't be abstract too since they can't be redefined.
ENDCLASS.
CLASS lcl_abstract IMPLEMENTATION.
  METHOD a_m1.
    WRITE :/ 'A_M1 Method in LCL_ABSTRACT class '.
  ENDMETHOD.
  METHOD a_m2.
    WRITE :/ 'A_M2 Method in LCL_ABSTRACT class '.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_subclass DEFINITION INHERITING FROM lcl_abstract.
  PUBLIC SECTION.
    METHODS: s_m1,
      s_m2,
      s_m3,
      a_m2 REDEFINITION,
      a_m3 REDEFINITION.
ENDCLASS.
CLASS lcl_subclass IMPLEMENTATION.
  METHOD s_m1.
    WRITE : / 'S_M1 Method in LCL_SUBCLASS class'.
  ENDMETHOD.
  METHOD s_m2.
    WRITE : / 'S_M2 Method in LCL_SUBCLASS class'.
  ENDMETHOD.
  METHOD s_m3.
    WRITE : / 'S_M3 Method in LCL_SUBCLASS class'.
  ENDMETHOD.
  METHOD a_m3.
    WRITE : / 'A_M3 Redefined method in LCL_SUBCLASS'.
  ENDMETHOD.
  METHOD a_m2.
    "you can now call a method from super class as below
* super->a_m2( ).
    WRITE : / 'A_M2 Non-abstract method Redefined in LCL_SUBCLASS'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA : lo_abs TYPE REF TO lcl_abstract.
  DATA(lo_sub) = NEW lcl_subclass( ).
  lo_Sub->s_m1( ).
  lo_Sub->s_m2( ).
  lo_Sub->s_m3( ).
  lo_sub->a_m1( ).
  lo_Sub->a_m2( ).
  lo_Sub->a_m3( ).
  ULINE.
  WRITE : /'Right afrter narrow casting'.
  ULINE.
  "you can't instantiate an abstract class.
  "so you get an object of the abstract class by casting.
  "When you cast subclass to abstract class,
  "you get to access the methods of the abstract class
  "and you can call them. Since now you have to call
  "a method from subclass with the superclass object.
  "you can only call it through dynamic calling.
  lo_abs = lo_sub.
  lo_abs->a_m1( ).
  lo_abs->a_m2( ).
  lo_abs->a_m3( ).
  "When you call a method dynamically, it decides in the run time
  "in which class the method belongs.
  "Call method has to be explicitly written in case of dynamic calling.
  CALL METHOD lo_abs->('S_M1').
  "Happy Learning.
