package com.wilsonvillerobotics.firstteamscouter;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.LinearLayout;

import java.util.ArrayList;

/**
 * Created by TomS on 2/26/2015.
 */
public class TransportContainer extends LinearLayout {
    private ArrayList<GameElement> gameElements;

    public TransportContainer(Context context) {
        super(context);
        this.gameElements = new ArrayList<GameElement>();
    }

    public TransportContainer(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.gameElements = new ArrayList<GameElement>();
    }

    public TransportContainer(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.gameElements = new ArrayList<GameElement>();
    }

    public ArrayList<GameElement> getGameElements() {
        return this.gameElements;
    }

    public int addGameElement(GameElement ge) {
        int index = this.gameElements.size();
        boolean added = false;
        if(ge != null) {
            this.gameElements.add(index, ge);
            //this.addView(ge.getImageView());
            this.addView(ge);
        }
        if(index == gameElements.size()) return -1;

        return index;
    }

    public GameElement getGameElement(int index) {
        if(index < this.gameElements.size()) {
            return this.gameElements.get(index);
        }
        return null;
    }

    public boolean deleteGameElement(int index) {
        Object o = null;
        if(index < this.gameElements.size()) {
            o = this.gameElements.remove(index);
        }
        return o == null;
    }

    public boolean deleteGameElement(GameElement ge) {
        return this.gameElements.remove(ge);
    }
}
