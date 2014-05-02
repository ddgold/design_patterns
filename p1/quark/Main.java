package quark;

//import SwingUtils.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class Main extends SwingApp {
   final static String base = "Base";
   final static String Ahead = "Ahead";
   final static String AspectJ = "AspectJ";
   final static String AML = "Aspectual Mixin Layers++";
   final static String General = "General";

   final static int  textWidth = 40;

   public static Object[] initChoices =
             { General, Ahead, AspectJ, AML};

   String activeModel = General;

   // initialize constants used in the application
   // REMEMBER -- make constants static!

   public void initConstants() {

   }

   // declare and initialize atomic components here

   JButton clear;
   JButton apply;
   JCheckBox hoa, gadvice, ladvice, intro, selected;
   JTextArea output;
   JTextField equation;
   JComboBox model;
   DefaultComboBoxModel comboModel;
   JTextArea program;

   public tree t;
   int  layerno;

   public void initAtoms() {
      t = new intro("p");
      layerno = 1;
      clear   = new JButton("Clear Module Expression");
      apply   = new JButton("Apply Quark");
      hoa     = new JCheckBox("hoa");
      hoa.setSelected(true);
      ladvice = new JCheckBox("local_advice");
      ladvice.setSelected(true);
      gadvice = new JCheckBox("global_advice");
      gadvice.setSelected(true);
      intro = new JCheckBox("intro");
      intro.setSelected(true);
      selected = new JCheckBox("selected");
      selected.setSelected(true);
      output= new JTextArea(3, textWidth);
      output.setText(t.toString());
      equation = new JTextField(textWidth);
      equation.setBorder(
    BorderFactory.createTitledBorder("Feature Expression") );
      equation.setText(base);
      comboModel = new DefaultComboBoxModel(initChoices);
      model = new JComboBox(comboModel);
      model.setBorder( BorderFactory.createTitledBorder("Model Type") );
      program = new JTextArea(3, textWidth);
      program.setText("p");
      program.setLineWrap(true);
   }

   // declare and initialize layout components here

   JPanel inputPanel;
   JPanel quarkPanel;
   JPanel clearPanel;
   JScrollPane outputScroll;
   JScrollPane programScroll;

   public void initLayout() {
      quarkPanel = new JPanel();
      quarkPanel.setLayout( new FlowLayout(FlowLayout.LEFT) );
      quarkPanel.add(hoa);
      quarkPanel.add(gadvice);
      quarkPanel.add(ladvice);
      quarkPanel.add(intro);
      quarkPanel.setBorder( BorderFactory.createTitledBorder("Quark Definition") );

      inputPanel = new JPanel();
      inputPanel.setLayout( new FlowLayout(FlowLayout.LEFT) );
      inputPanel.add(apply);
      inputPanel.add(quarkPanel);
      inputPanel.add(model);
      inputPanel.add(selected);

      clearPanel = new JPanel();
      clearPanel.setLayout( new FlowLayout(FlowLayout.LEFT) );
      clearPanel.add(clear);

      outputScroll = new JScrollPane(output);
      outputScroll.setPreferredSize(new Dimension(750,100));
      outputScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
      outputScroll.setBorder( BorderFactory.createTitledBorder("Module Expression") );
      programScroll = new JScrollPane(program);
      programScroll.setPreferredSize(new Dimension(750,80));
      programScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
      programScroll.setBorder( BorderFactory.createTitledBorder("Program") );
   }

   // initialize ContentPane here

   public void initContentPane() {
      ContentPane = new JPanel();
      ContentPane.setLayout( new BoxLayout(ContentPane, BoxLayout.Y_AXIS) );
      ContentPane.add(inputPanel);
      ContentPane.add(equation);
      ContentPane.add(outputScroll);
      ContentPane.add(programScroll);
      ContentPane.add(clearPanel);
   }

   // initialize listeners here

   public void initListeners() {
      clear.addActionListener( new ActionListener() {
         public void actionPerformed( ActionEvent e ) {
            t = new intro("p");
        layerno = 1;
        output.setText(t.toString());
        program.setText( t.eval("") );
        equation.setText(base);
        updateQuarkPanel();
     }
      });
      apply.addActionListener( new ActionListener() {
         public void actionPerformed( ActionEvent e ) {
            if (hoa.isSelected()) { t = t.apply( new hoa("h" +layerno)); }
            if (ladvice.isSelected()) { t = t.apply( new advice("a"+layerno)); }
            if (intro.isSelected()) {t = t.apply( new intro( "i"+layerno) ); }
            if (gadvice.isSelected()) { t = t.apply( new gadvice("g"+layerno)); }
        if (hoa.isSelected() || gadvice.isSelected()
            || ladvice.isSelected() || intro.isSelected()) {
           hoa.setSelected(false);
           gadvice.setSelected(false);
           ladvice.setSelected(false);
           intro.setSelected(false);
           equation.setText("F"+layerno+"("+equation.getText()+")");
           layerno++;
           output.setText(output.getText() + "\n" + t.toString() );
           program.setText( t.eval("") );
           updateQuarkPanel();
        }
     }
      });
      model.addActionListener( new ActionListener(){
         public void actionPerformed(ActionEvent e) {
            JComboBox cb = (JComboBox) e.getSource();
            activeModel = (String) cb.getSelectedItem();
        updateQuarkPanel();
         }
      } );
      selected.addItemListener( new ItemListener() {
         public void itemStateChanged(ItemEvent e) {
         updateQuarkPanel();
     }
      });
   }

   // place in this method any action for exiting application

   public void applicationExit() {

   }

   public Main() { super(); }

   public Main(String AppTitle) { super(AppTitle); }

   public static void main(String[] args) {
      new Main("quark.Main");
   }

   public void updateQuarkPanel() {
      boolean s = selected.isSelected();
      if (activeModel.equals(Ahead)) {
         intro.setEnabled(true);
         intro.setSelected(s);
     ladvice.setEnabled(true);
     ladvice.setSelected(s);
     gadvice.setEnabled(false);
     gadvice.setSelected(false);
     hoa.setEnabled(false);
     hoa.setSelected(false);
      } else
      if (activeModel.equals(AspectJ)) {
     intro.setEnabled(true);
     intro.setSelected(s);
     ladvice.setEnabled(false);
     ladvice.setSelected(false);
     gadvice.setEnabled(true);
     gadvice.setSelected(s);
     hoa.setEnabled(false);
     hoa.setSelected(false);
      } else
      if (activeModel.equals(AML)) {
         intro.setEnabled(true);
     intro.setSelected(s);
     ladvice.setEnabled(false);
     ladvice.setSelected(false);
     gadvice.setEnabled(true);
     gadvice.setSelected(s);
     hoa.setEnabled(true);
     hoa.setSelected(s);
      } else {
         intro.setEnabled(true);
     intro.setSelected(s);
         ladvice.setEnabled(true);
         ladvice.setSelected(s);
         gadvice.setEnabled(true);
         gadvice.setSelected(s);
         hoa.setEnabled(true);
         hoa.setSelected(s);
      }
   }

   // nested class declaration
   //
   class CheckBoxListener implements ItemListener {
      public void itemStateChanged(ItemEvent e) {

         JCheckBox selectedItem = (JCheckBox) e.getItemSelectable();

         if (selectedItem == selected)
         updateQuarkPanel();
      }
   }

}
