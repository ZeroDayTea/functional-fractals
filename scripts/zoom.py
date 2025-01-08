import os
import subprocess
import math

WIDTH = 1920
HEIGHT = 1080
RADIUS = 2.0
MAX_ITER = 512
FPS = 20
SECONDS = 20
ZOOM_START = 1.5
ZOOM_END = 0.00001

def main():
   total_frames = FPS * SECONDS
   frames_dir = "frames"
   os.makedirs(frames_dir, exist_ok=True)
   x_center, y_center = (-1.769469557, -0.003115621)

   aspect_ratio = WIDTH / HEIGHT

   for i in range(total_frames):
       t = i / (total_frames - 1)
       curr_zoom = ZOOM_START * (ZOOM_END / ZOOM_START) ** t
       x_min = x_center - curr_zoom * aspect_ratio
       x_max = x_center + curr_zoom * aspect_ratio
       y_min = y_center - curr_zoom
       y_max = y_center + curr_zoom

       out_file = os.path.join(frames_dir, f"frame_{i:04d}.png")

       cmd = [
           "../haskell/hs-fractals",
           str(WIDTH),
           str(HEIGHT),
           str(MAX_ITER),
           str(RADIUS),
           str(x_min),
           str(x_max),
           str(y_min),
           str(y_max),
           out_file
       ]

       print(f"Generating frame {i+1}/{total_frames} and zoom {curr_zoom}")
       subprocess.run(cmd, check=True)

   video_file = "mandelbrot_zoom.mp4"
   ffmpeg_cmd = [
       "ffmpeg",
       "-y",
       "-framerate", str(FPS),
       "-i", os.path.join(frames_dir, "frame_%04d.png"),
       "-pix_fmt", "yuv420p",
       video_file
   ]

   print(f"Creating video {video_file}")
   subprocess.run(ffmpeg_cmd, check=True)

if __name__ == "__main__":
    main()
