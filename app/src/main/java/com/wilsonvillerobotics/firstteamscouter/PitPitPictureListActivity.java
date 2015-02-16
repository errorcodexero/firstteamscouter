package com.wilsonvillerobotics.firstteamscouter;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import android.annotation.SuppressLint;
import android.database.SQLException;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.provider.MediaStore;
import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PictureDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitPicturesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;


/**
 * Created by TomS on 2/15/2015.
 * http://blog.andolasoft.com/2013/06/how-to-show-captured-images-dynamically-in-gridview-layout.html
 */
public class PitPitPictureListActivity extends Activity implements OnClickListener {
    private GridView gridView;
    private GridViewAdapter customGridAdapter;

    private PitPicturesDBAdapter ppDBAdapter;
    private PictureDataDBAdapter pdDBAdapter;

    private long ppID;
    private long teamId;
    private String teamNumber;
    private long pitId;

    Button btnTakePicture = null;
    final int CAMERA_CAPTURE = 1;
    private Uri picUri;
    private DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    //private GridView grid;
    private List<String> listOfImagesPath;

    //public static final String pitPicturesImagePath = Environment.getExternalStorageDirectory().getAbsolutePath() + "/GridViewDemo/";
    public File filePath;
    public String pitPicturesImagePath;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_pit_pit_picture_list);

        filePath = getExternalFilesDir(null);
        pitPicturesImagePath = filePath.getAbsolutePath() + "/images/";

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            ppDBAdapter = new PitPicturesDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            ppDBAdapter = null;
        }

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            pdDBAdapter = new PictureDataDBAdapter(this.getBaseContext()).open();
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
            gridView.setAdapter(new ImageListAdapter(this,listOfImagesPath));
        }
    }

    private void processIntent() {
        Intent intent = getIntent();
        this.teamId = intent.getLongExtra("team_id", -1);
        this.teamNumber = intent.getStringExtra("team_number");
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
                String imgcurTime = dateFormat.format(new Date());

                File imageDirectory = new File(pitPicturesImagePath);
                if(!imageDirectory.exists()) {
                    imageDirectory.mkdirs();
                }

                File[] imageFileList = imageDirectory.listFiles(new ImageFilenameFilter(".jpg"));
                int fileCount = imageFileList.length;

                //String _path = "PitPicture" + imgcurTime + ".jpg";
                String _path = teamNumber + "_PitPicture_" + fileCount + ".jpg";
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
                    gridView.setAdapter(new ImageListAdapter(this,listOfImagesPath));
                }
            }
        }
    }

    private List<String> RetrieveCapturedImagePath() {
        List<String> tFileList = new ArrayList<String>();
        File imageDirectory = new File(pitPicturesImagePath);
        if (imageDirectory.exists()) {
            File[] imageFileList = imageDirectory.listFiles(new ImageFilenameFilter(".jpg"));
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
        public ImageListAdapter(Context c, List<String> thePic)
        {
            context = c;
            imgPic = thePic;
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
                fs = new FileInputStream(new File(imgPic.get(position).toString()));

                if(fs!=null) {
                    bm=BitmapFactory.decodeFileDescriptor(fs.getFD(), null, bfOptions);
                    imageView.setImageBitmap(bm);
                    imageView.setId(position);
                    imageView.setLayoutParams(new GridView.LayoutParams(200, 160));
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
    }

    public class ImageFilenameFilter implements FilenameFilter {
        String ext;

        public ImageFilenameFilter(String ext) {
            this.ext = ext;

        }

        @SuppressLint("DefaultLocale")
        @Override
        public boolean accept(File dir, String filename) {
            //If you want to perform a case-insensitive search
            return filename.toLowerCase().endsWith(ext.toLowerCase());
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
        setContentView(R.layout.activity_pit_pit_picture_list);

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            ppDBAdapter = new PitPicturesDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            ppDBAdapter = null;
        }

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            pdDBAdapter = new PictureDataDBAdapter(this.getBaseContext()).open();
        } catch(SQLException e) {
            e.printStackTrace();
            pdDBAdapter = null;
        }



        gridView = (GridView) findViewById(R.id.gridView);
        customGridAdapter = new GridViewAdapter(this, R.layout.activity_pit_pit_picture_list, getData());
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