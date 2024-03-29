package Notepad;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;

class UndoAction extends AbstractAction {

    Main notepad;

    public UndoAction(Main notepad) {
        super("Undo");
        putValue(Action.SMALL_ICON,
                new ImageIcon(this.getClass().getResource("")));
        setEnabled(false);
        this.notepad = notepad;
    }

    public void actionPerformed(ActionEvent e) {
        try {
            notepad.undo.undo();
        } catch (CannotUndoException ex) {
            System.out.println("Unable to undo: " + ex);
            ex.printStackTrace();
        }
        update();
        notepad.redoAction.update();
    }

    protected void update() {
        if (notepad.undo.canUndo()) {
            setEnabled(true);
            putValue("Undo", notepad.undo.getUndoPresentationName());
        } else {
            setEnabled(false);
            putValue(Action.NAME, "Undo");
        }
    }
}
