const animatedCred = document.querySelector('.photo-cred');


const animatedCredTimeline = new ScrollTimeline({
  scrollOffsets: [
    { target: animatedCred, edge: "end", threshold: "1" },
    { target: animatedCred, edge: "start", threshold: "1" },
  ],
});


animatedCred.animate(
  {
    transform: ["opacity: 0;", "opacity: 1;"],
  },
  {
    duration: 1,
    timeline: animatedCredTimeline
  }
);