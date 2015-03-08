package com.wilsonvillerobotics.firstteamscouter;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.database.SQLException;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.view.ContextMenu;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PictureDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitPicturesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotPicturesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ItemType;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;


/**
 * Created by TomS on 2/15/2015.
 * http://blog.andolasoft.com/2013/06/how-to-show-captured-images-dynamically-in-gridview-layout.html
 */
public class PictureListActivity extends Activity implements OnClickListener {
    private GridView gridView;

    private RobotPicturesDBAdapter rpDBAdapter;
    private PitPicturesDBAdapter   ppDBAdapter;
    private PictureDataDBAdapter   pdDBAdapter;

    private ImageView clickedImage;

    private long rpID;
    private long teamId;
    private String teamNumber;
    private long robotId;

    Button btnTakePicture = null;
    final int CAMERA_CAPTURE = 1;
    private Uri picUri;
    private DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss");
    private List<String> listOfImagesPath;
    private String imageNamePrefix;

    public File filePath;
    public String imagesPath;
    private ItemType itemType;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_picture_list);

        this.processIntent();

        this.clickedImage = null;

        filePath = getExternalFilesDir(null);
        imagesPath = filePath.getAbsolutePath() + "/images/";
        if(itemType != ItemType.ALL && itemType != ItemType.NONE) {
            imagesPath += itemType.getName().toLowerCase() + "/";
        }

        this.imageNamePrefix = this.teamNumber + "_" + this.itemType.getName() + "_";

        try {
            FTSUtilities.printToConsole("PictureListActivity::onCreate : OPENING DB\n");
            switch(this.itemType) {
                case ROBOT:
                    rpDBAdapter = new RobotPicturesDBAdapter(this).openForWrite();
                    ppDBAdapter = null;
                    break;
                case PIT:
                    rpDBAdapter = null;
                    ppDBAdapter = new PitPicturesDBAdapter(this).openForWrite();
                    break;
                case ALL:
                    rpDBAdapter = new RobotPicturesDBAdapter(this).openForWrite();
                    ppDBAdapter = new PitPicturesDBAdapter(this).openForWrite();
                    break;
                default:
                case NONE:
                    rpDBAdapter = null;
                    ppDBAdapter = null;
            }
        } catch(SQLException e) {
            e.printStackTrace();
            rpDBAdapter = null;
            ppDBAdapter = null;
        }

        try {
            FTSUtilities.printToConsole("PictureListActivity::onCreate : OPENING DB\n");
            pdDBAdapter = new PictureDataDBAdapter(this).openForWrite();
        } catch(SQLException e) {
            e.printStackTrace();
            pdDBAdapter = null;
        }

        btnTakePicture = (Button)findViewById(R.id.btnTakePicture);
        btnTakePicture.setOnClickListener(this);
        gridView = ( GridView) findViewById(R.id.gridImages);

        listOfImagesPath = null;
        listOfImagesPath = RetrieveCapturedImagePath();
        if(listOfImagesPath!=null){
            gridView.setAdapter(new ImageListAdapter(this,listOfImagesPath, this.itemType));
        }
    }

    private void processIntent() {
        Intent intent = getIntent();
        this.teamId = intent.getLongExtra("team_id", -1);
        this.teamNumber = intent.getStringExtra("team_number");
        this.itemType = ItemType.getItemTypeByName(intent.getStringExtra("item_type"));
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
// Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }

    @Override
    public void onClick(View arg0) {
// TODO Auto-generated method stub
        if (arg0.getId() == R.id.btnTakePicture) {

            try {
                //use standard intent to capture an image
                Intent captureIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                //we will handle the returned data in onActivityResult
                startActivityForResult(captureIntent, CAMERA_CAPTURE);
            } catch(ActivityNotFoundException anfe){
                //display an error message
                String errorMessage = "Whoops - your device doesn't support capturing images!";
                Toast toast = Toast.makeText(this, errorMessage, Toast.LENGTH_SHORT);
                toast.show();
            }
        }

    }

    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode == RESULT_OK) {
            //user is returning from capturing an image using the camera
            if(requestCode == CAMERA_CAPTURE){
                Bundle extras = data.getExtras();
                Bitmap thePic = extras.getParcelable("data");
                StringBuffer imgCurTime = new StringBuffer();
                dateFormat.format(new Date(), imgCurTime, new FieldPosition(0));

                File imageDirectory = new File(imagesPath);
                if(!imageDirectory.exists()) {
                    imageDirectory.mkdirs();
                }

                String _path = this.imageNamePrefix + imgCurTime + ".jpg";
                File newImage = new File(imageDirectory, _path);
                try {
                    newImage.createNewFile();
                    FileOutputStream out = new FileOutputStream(newImage);
                    thePic.compress(Bitmap.CompressFormat.JPEG, 90, out);
                    out.close();
                } catch (FileNotFoundException e) {
                    e.getMessage();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                listOfImagesPath = null;
                listOfImagesPath = RetrieveCapturedImagePath();
                if(listOfImagesPath!=null){
                    gridView.setAdapter(new ImageListAdapter(this,listOfImagesPath, this.itemType));
                }
            }
        }
    }

    private List<String> RetrieveCapturedImagePath() {
        List<String> tFileList = new ArrayList<String>();
        File imageDirectory = new File(imagesPath);
        if (imageDirectory.exists() || imageDirectory.mkdirs()) {
            File[] imageFileList = imageDirectory.listFiles(new ImageFilenameFilter(this.imageNamePrefix, ".jpg"));
            Arrays.sort(imageFileList);

            for(File f : imageFileList){
                if(f.isDirectory())
                    continue;
                tFileList.add(f.getPath());
            }
        }
        return tFileList;
    }

    public class ImageListAdapter extends BaseAdapter
    {
        private Context context;
        private List<String> imgPic;
        private ItemType imgType;
        public ImageListAdapter(Context c, List<String> thePic, ItemType type)
        {
            context = c;
            imgPic = thePic;
            imgType = type;
        }
        public int getCount() {
            if(imgPic != null)
                return imgPic.size();
            else
                return 0;
        }

        //---returns the ID of an item---
        public Object getItem(int position) {
            return position;
        }

        public long getItemId(int position) {
            return position;
        }

        //---returns an ImageView view---
        public View getView(int position, View convertView, ViewGroup parent)
        {
            ImageView imageView;
            BitmapFactory.Options bfOptions=new BitmapFactory.Options();
            bfOptions.inDither=false;                     //Disable Dithering mode
            bfOptions.inPurgeable=true;                   //Tell to gc that whether it needs free memory, the Bitmap can be cleared
            bfOptions.inInputShareable=true;              //Which kind of reference will be used to recover the Bitmap data after being clear, when it will be used in the future
            bfOptions.inTempStorage=new byte[32 * 1024];
            if (convertView == null) {
                imageView = new ImageView(context);
                imageView.setLayoutParams(new GridView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));
                imageView.setPadding(0, 0, 0, 0);
            } else {
                imageView = (ImageView) convertView;
            }
            FileInputStream fs = null;
            Bitmap bm;
            try {
                String path = imgPic.get(position).toString();
                fs = new FileInputStream(new File(path));

                if(fs!=null) {
                    bm=BitmapFactory.decodeFileDescriptor(fs.getFD(), null, bfOptions);
                    imageView.setImageBitmap(bm);
                    imageView.setId(position);
                    imageView.setLayoutParams(new GridView.LayoutParams(200, 160));
                    imageView.setOnTouchListener(new MyViewTouchListener());
                    imageView.setTag(R.id.TAG_PATH_ID, path);
                    imageView.setTag(R.id.TAG_ITEM_TYPE_ID, itemType);
                    registerForContextMenu(imageView);
                }
            } catch (IOException e) {
                e.printStackTrace();
            } finally{
                if(fs!=null) {
                    try {
                        fs.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
            return imageView;
        }

        private final class MyViewTouchListener implements View.OnTouchListener {
            @Override
            public boolean onTouch(View view, MotionEvent motionEvent) {
                if (motionEvent.getAction() == MotionEvent.ACTION_DOWN) {
                    //ClipData data = ClipData.newPlainText("", "");
                    //View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
                    //view.startDrag(data, shadowBuilder, view, 0);
                    view.showContextMenu();
                    return true;
                } else if (motionEvent.getAction() == MotionEvent.ACTION_MOVE) {
                    //view.setVisibility(View.GONE);
                    return true;
                } else return motionEvent.getAction() == MotionEvent.ACTION_UP;
            }
        }
    }

    public class ImageFilenameFilter implements FilenameFilter {
        String filePrefix;
        String ext;

        public ImageFilenameFilter(String filePrefix, String ext) {
            this.filePrefix = filePrefix;
            this.ext = ext;

        }

        @SuppressLint("DefaultLocale")
        @Override
        public boolean accept(File dir, String filename) {
            //If you want to perform a case-insensitive search
            boolean matches = filename.toLowerCase().startsWith(filePrefix.toLowerCase());
            matches &= filename.toLowerCase().endsWith(ext.toLowerCase());
            return matches;
        }
    }

    /*
     * This method is called when a context menu for the view about to be shown.
     */
    @Override
    public void onCreateContextMenu(ContextMenu menu, View v,
                                    ContextMenu.ContextMenuInfo menuInfo) {

        super.onCreateContextMenu(menu, v, menuInfo);

        if(v.getClass() == ImageView.class) this.clickedImage = (ImageView)v;

        if(this.clickedImage != null) {
            switch((ItemType)clickedImage.getTag(R.id.TAG_ITEM_TYPE_ID)) {
                case ROBOT:
                    menu.setHeaderIcon(R.drawable.robot_50x50);
                    menu.setHeaderTitle("Robot Image");
                    menu.add(0, 0, 0, "Cancel");
                    menu.add(0, 1, 0, "Delete Image");
                    menu.add(0, 2, 0, "Set As Main Image");
                    break;
                case PIT:
                    menu.setHeaderIcon(R.drawable.gray_tote_side_up_50x24);
                    menu.setHeaderTitle("Pit Image");
                    menu.add(1, 0, 0, "Cancel");
                    menu.add(1, 1, 0, "Delete Image");
                    break;
                case TEAM:
                    menu.setHeaderIcon(R.drawable.green_can_top_down_40x40);
                    menu.setHeaderTitle("Team Image");
                    menu.add(2, 0, 0, "Cancel");
                    menu.add(2, 1, 0, "Delete Image");
                    menu.add(2, 2, 0, "Set As Main Image");
                    break;
            }


        }
    }

    /*
    * This method is called when an item in a context menu is selected.
    *
    */
    @Override
    public boolean onContextItemSelected(MenuItem item) {
        switch(item.getGroupId()) {
            case 0: // Robot image actions
            case 1: // Pit image actions
            case 2: // Team image actions
                switch (item.getItemId()) {
                    case 0:
                        //cancel
                        break;
                    case 1: // delete
                        if(clickedImage != null) deleteImage();
                        clickedImage = null;
                        break;
                }
                break;
        }
        return true;
    }

    private void deleteImage() {
        if(clickedImage != null) {
            String path = (String)clickedImage.getTag(R.id.TAG_PATH_ID);
            if(path != null) {
                FTSUtilities.printToConsole("Deleting Image from ImageView: " + path);

                if(listOfImagesPath != null) {
                    GridView gv = (GridView)clickedImage.getParent();
                    ImageListAdapter ila = (ImageListAdapter)gv.getAdapter();
                    listOfImagesPath.remove(path);
                    ila.notifyDataSetChanged();
                }

                File f = new File(path);
                String deleted = "";
                if(f.delete()) {
                    deleted = " deleted.";
                } else {
                    deleted = " NOT deleted.";
                }
                Toast.makeText(getBaseContext(), "File" + deleted, Toast.LENGTH_LONG).show();

                clickedImage = null;
            }
        }
    }
}
/*
public class PitPitPictureListActivity extends Activity {
    private GridView gridView;
    private GridViewAdapter customGridAdapter;

    private PitPicturesDBAdapter ppDBAdapter;
    private PictureDataDBAdapter pdDBAdapter;

    private long ppID;
    private long teamId;
    private long pitId;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_picture_list);

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            ppDBAdapter = new PitPicturesDBAdapter(this.getBaseContext()).openForWrite();
        } catch(SQLException e) {
            e.printStackTrace();
            ppDBAdapter = null;
        }

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            pdDBAdapter = new PictureDataDBAdapter(this.getBaseContext()).openForWrite();
        } catch(SQLException e) {
            e.printStackTrace();
            pdDBAdapter = null;
        }



        gridView = (GridView) findViewById(R.id.gridView);
        customGridAdapter = new GridViewAdapter(this, R.layout.activity_picture_list, getData());
        gridView.setAdapter(customGridAdapter);
    }

    private ArrayList getData() {
        final ArrayList imageItems = new ArrayList();
        // retrieve String drawable array
        //TypedArray imgs = getResources().obtainTypedArray(R.array.image_ids);

        ArrayList<Long> imgs = ppDBAdapter.getAllPictureIDsForPit(pitId);
        //for (int i = 0; i < imgs.length(); i++) {
        for(Long id : imgs) {
            Bitmap bitmap = BitmapFactory.decodeResource(this.getResources(),
                    imgs.getResourceId(i, -1));
            imageItems.add(new ImageItem(bitmap, "Image#" + i));
        }

        return imageItems;
    }

    private void processIntent() {
        Intent intent = getIntent();
        this.teamId = intent.getLongExtra("team_id", -1);
        this.teamNumber = intent.getStringExtra("team_number");
    }

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onRestart() {
        super.onRestart();
    }

    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onPause() {
        super.onPause();
    }

    @Override
    protected void onStop() {
        super.onStop();
    }
}
*/