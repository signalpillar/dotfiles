// Exercises, instructions and illustrations are from Work From Home Desks NZ:
// https://www.workfromhomedesks.co.nz/blogs/blog/standing-desk-exercises
// Timings follow the article's rep/hold counts (lower bound of each range).

export const ARTICLE = {
  title: "6 Exercises You Can Do At Your Standing Desk",
  publisher: "Work From Home Desks NZ",
  url: "https://www.workfromhomedesks.co.nz/blogs/blog/standing-desk-exercises",
  hero: "https://cdn.shopify.com/s/files/1/0623/3361/6387/t/7/assets/-standing-desk--ergonomic-desk--healthy-work--wfh-deskheader-1644526555091_500x.jpg",
};

export const EXERCISES = [
  {
    id: "calf-raises",
    title: "Standing Calf Raises",
    image: "https://cdn.shopify.com/s/files/1/0623/3361/6387/t/7/assets/-standing-desk--ergonomic-desk--healthy-work--wfh-desk-1644526555095_500x.jpg",
    benefit: "Calf raises use your body weight to strengthen and tone the gastrocnemius and soleus.",
    instructions:
      "Lightly holding the edge of your desk for balance, rise up onto your toes, lifting your heels off the floor. Go slowly — take two counts going up, hold for two, then two counts coming back down. Do 15 to 25 reps (you can do both legs together or just do one leg at a time to make it more challenging).",
    source: { label: "WebMD", url: "https://www.webmd.com/fitness-exercise/strengthening-calf-muscles" },
    cue: "Fifteen slow reps. Hold the desk lightly.",
    repeat: 15,
    steps: [
      ["Rise up onto your toes", 2],
      ["Hold at the top", 2, "hold"],
      ["Lower slowly", 2, "down"],
    ],
  },
  {
    id: "leg-extensions",
    title: "Standing Leg Extensions",
    image: "https://cdn.shopify.com/s/files/1/0623/3361/6387/t/7/assets/-standing-desk--ergonomic-desk--healthy-work--wfh-desk2-1644526555065_500x.jpg",
    benefit: "Leg extensions strengthen the patellar ligament and quadriceps attachment for the knee.",
    instructions:
      "Engage your core (tighten abs) — this prevents your lower back from arching too much — then extend one of your legs straight behind you, engaging your glutes as you go. Do 12 to 25 reps.",
    source: { label: "HealthifyMe", url: "https://www.healthifyme.com/blog/five-reasons-why-leg-extensions-work/" },
    cue: "Twelve reps each leg. Tighten your core. Left leg first.",
    sides: ["Left", "Right"],
    repeat: 12,
    steps: [
      ["Extend the leg back, squeeze the glute", 2],
      ["Return to standing", 1.5, "back"],
    ],
  },
  {
    id: "hamstring-curl",
    title: "Standing Hamstring Curl",
    image: "https://cdn.shopify.com/s/files/1/0623/3361/6387/t/7/assets/-standing-desk--ergonomic-desk--healthy-work--wfh-desk3-1644526555069_500x.jpg",
    benefit: "Hamstring curls engage your hamstrings and glutes, which makes them stronger.",
    instructions:
      "Bend one knee and bring your foot toward your glute, then release back down. Do 12 to 25 reps on each side.",
    source: { label: "Healthline", url: "https://www.healthline.com/health/hamstring-curls" },
    cue: "Twelve reps each side. Left leg first.",
    sides: ["Left", "Right"],
    repeat: 12,
    steps: [
      ["Curl the heel toward your glute", 1.5],
      ["Release back down", 1.5, "down"],
    ],
  },
  {
    id: "calf-stretch",
    title: "Standing Calf Stretch",
    image: "https://cdn.shopify.com/s/files/1/0623/3361/6387/t/7/assets/-standing-desk--ergonomic-desk--healthy-work--wfh-desk4-1644526555074_500x.jpg",
    benefit: "A calf stretch exercise can help prevent an Achilles tendon rupture.",
    instructions:
      "Move closer to one leg of your desk, lift the ball of one foot off the floor, and place it against the leg (keep your heel on the floor). Lean your body into the desk and feel the stretch in your calf. Hold for 20 to 30 seconds and repeat on the other side.",
    source: {
      label: "Mayo Clinic",
      url: "https://www.mayoclinic.org/diseases-conditions/achilles-tendon-rupture/multimedia/calf-stretch-exercise/img-20008632",
    },
    cue: "Thirty seconds each side. Ball of the foot against the desk leg.",
    prep: ["Place the ball of your left foot against the desk leg", 5, "Left foot against the desk leg."],
    sides: ["Left", "Right"],
    switchSeconds: 6,
    steps: [["Lean in and breathe into the stretch", 30, "Lean in. Breathe."]],
  },
  {
    id: "chest-stretch",
    title: "Standing Chest Stretch",
    image: "https://cdn.shopify.com/s/files/1/0623/3361/6387/t/7/assets/-standing-desk--ergonomic-desk--healthy-work--wfh-desk5-1644526555078_500x.jpg",
    benefit: "This chest stretch lengthens chest muscles so that healthier positioning feels natural and comfortable.",
    instructions:
      "Clasp your hands together behind your back, straighten your arms, and lift them up while drawing your shoulder blades down. You'll feel your chest open, which should feel great if you find yourself hunching forward at your desk. Hold for 20 to 30 seconds and repeat.",
    source: { label: "Healthline", url: "https://www.healthline.com/health/fitness-exercise/pectoral-chest-stretch" },
    cue: "Two holds of twenty-five seconds.",
    steps: [
      ["Clasp hands behind, lift the arms, open the chest", 25, "Clasp your hands. Lift. Open the chest."],
      ["Release and shake out", 5, "Release."],
      ["Clasp and lift again", 25, "Once more. Lift."],
    ],
  },
  {
    id: "twist",
    title: "Standing Twist / Back Stretch",
    image: "https://cdn.shopify.com/s/files/1/0623/3361/6387/t/7/assets/-standing-desk--ergonomic-desk--healthy-work--wfh-desk6-1644526555082_500x.jpg",
    benefit:
      "Twisting can activate the muscles around the lumbar spine and abdominal core, increasing stability as well as blood flow and oxygenation to the lower back.",
    instructions:
      "Bend your right knee and lift it up to hip height, so that you're balancing on your left leg. Place your left hand on your right knee and twist your torso toward the right. Hold for 10 seconds, then repeat on the other side.",
    source: { label: "Yoga Journal", url: "https://www.yogajournal.com/poses/yoga-antomy-prevent-low-back-pain-twists/" },
    cue: "Ten seconds each side, twice. Right knee up first.",
    sides: ["Right", "Left", "Right", "Left"],
    steps: [["Knee to hip height, hand on knee, twist", 10, "Knee up. Twist."]],
  },
];

// Mindful bookends (not from the article).
export const ARRIVE = {
  id: "arrive",
  title: "Arrive",
  mindful: true,
  instructions:
    "Soften your knees, let your shoulders drop and rest your eyes somewhere away from the screen. Breathe in a square: in, hold, out, rest.",
  cue: "Eyes off the screen. Breathe in a square.",
  repeat: 3,
  steps: [
    ["Breathe in", 4, "In"],
    ["Hold", 4],
    ["Breathe out", 4, "Out"],
    ["Rest", 4],
  ],
};

export const RETURN = {
  id: "return",
  title: "Return",
  mindful: true,
  instructions: "Before you look back at the screen, choose the one thing you will give your attention to next.",
  cue: "Choose your next single focus.",
  steps: [
    ["Roll your shoulders, shake out your hands", 8, "Roll your shoulders."],
    ["Name your next single focus", 10, "What is the one thing next?"],
  ],
};
