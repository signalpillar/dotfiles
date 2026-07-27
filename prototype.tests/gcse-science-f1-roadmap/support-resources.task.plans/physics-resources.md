# Physics Resource Publication Plan

## Outcome

Publish complete support bundles for P1 through P35 while preserving every published M1 through M20 bundle and its catalog mapping byte for byte.
The finished physics catalog will contain all 35 manifest physics topics, and every topic will have a concise board-neutral summary, an interactive tutor prompt, at least two reviewed free supporting resources, and one separate focused English Wikipedia link.
This slice will add physics content and catalog mappings only, without changing the embedded manifest, its 121 topic records or 189 prerequisite edges, `papers.json`, runtime code, validators, audit tooling, fixtures, or existing tests.
The catalog-count generalization already completed before this slice is the only accepted test change and must not be edited again here.

## Authority And Safety Boundaries

The embedded manifest remains authoritative for every P1-P35 ID, title, specification sentence, prerequisite edge, check, stage, driver, and F1 or DIY example.
The bundle prose may explain the manifest scope but must not copy manifest-only fields into `topic.json` or claim additional exam-board content.
The final live destination after every redirect remains authoritative for resource title, publisher, subject coverage, access conditions, and link metadata.
Physics claims must be supported by inspected credible sources, and an evidence gap must leave the topic unpublished rather than invite a plausible invention.
The prose and prompt must distinguish simplified models from physical reality where literal interpretation would create a misconception, including particle diagrams, ideal circuits, ray diagrams, gas models, field lines, perfect transformers, circular orbits, and cosmological models.
Every calculation and worked explanation must define the sign convention or direction where relevant, preserve vector direction, convert to consistent compatible units before substitution, use SI where the question or relationship requires it, carry units through intermediate work, and check the final unit and physical sense.
No resource may invent motorsport performance figures, engineering causation, medical advice, electrical procedures, radiation procedures, fuel-handling procedures, or practical methods beyond the exact manifest examples and inspected safe educational evidence.
Practical content must describe school-level planning and interpretation rather than instructing a learner to handle mains electricity, high voltage, fuel vapour, ionising sources, hot apparatus, pressurised containers, lasers, or other hazardous equipment without qualified supervision and an approved method.
The content must remain board-neutral and may not label a formula, practical, convention, or depth as required by a named board unless the manifest or an inspected official source establishes that claim and the bundle still fits the shared manifest scope.

## Evidence Delegation And Independence

Assign each bounded batch below to one evidence researcher so related terminology, equations, models, and examples are checked consistently.
Require the researcher to read every assigned manifest record and produce an unpublished draft plus an evidence log for each topic under `/var/folders/f8/bb6ngcg16k5b6sd83h72xy880000gn/T/opencode/gcse-physics-resource-drafts/` so another session can resume without conversational context or an undisclosed temporary path.
Each evidence log must record the candidate URL, final HTTPS URL after redirects, page title, publisher, access and gate result, exact topic relevance, claims supported, limitations or model qualifications, research execution ID, draft execution ID, reviewer execution ID, review timestamp, and approval or rejection verdict.
Require at least four supporting-resource candidates and at least two Wikipedia candidates per topic so rejection does not force weak publication choices.
Assign every completed topic to a reviewer who did not research or draft that topic.
The independent reviewer must reopen every declared supporting destination plus the Wikipedia destination, compare prose and prompt line by line with the manifest and evidence log, run the topic gate, and approve or reject that topic separately.
No research or drafting execution may approve its own topic, no group-level approval may substitute for per-topic review, and a failing topic remains outside the published tree while passing siblings continue through review.
The listed F1 and DIY examples are the only permitted factual real-world contexts for a topic, but neutral abstract objects, diagrams, fictional datasets, and context-free numerical questions may be created when they add no factual or practical claim beyond the manifest scope.

## Bounded Research Batches

### Batch A: Particle Model, Materials, And Fields

- P16 has exact scope limited to the particle model for solids, liquids, gases, changes of state, and conservation of mass; its prerequisite diagnostic asks for no prior roadmap topic but checks that the learner can distinguish a substance from its particles; its misconception checks target particles expanding, disappearing, becoming a new substance, or gaining mass during a state change; and its permitted factual contextual examples are coolant density changing as it heats, water in wet tyres changing state, melting and setting wax, and evaporating paint solvent.
- P17 has exact scope limited to `density = mass / volume` and density measurement for regular solids, irregular solids, and liquids; its prerequisite diagnostic checks M2 unit conversion and P16 particle-state understanding; its misconception checks target confusing mass with density, reading displaced water as mass, omitting the initial cylinder reading, and mixing `g/cm^3` with `kg/m^3`; and its permitted factual contextual examples are equal-mass tungsten and aluminium ballast blocks and water displacement of an irregular metal fitting.
- P18 has exact scope limited to particle-collision explanations of gas pressure and relationships among pressure, volume, and absolute temperature for a fixed gas mass; its prerequisite diagnostic checks P16 gas-particle motion and M4 direct and inverse proportionality; its misconception checks target using Celsius as absolute temperature, assuming every change holds another variable constant, treating pressure as stored material, and ignoring fixed mass; and its permitted factual contextual examples are F1 tyre pressure rising as gas warms, tyre volume changing slightly under load, and the warning not to heat a sealed aerosol can.
- P19 has exact scope limited to `F = kx`, the limit of proportionality, and elastic potential energy; its prerequisite diagnostic checks P1 force magnitude and direction and M14 substitution and equation solving; its misconception checks target treating every force-extension graph as permanently linear, confusing elastic limit with limit of proportionality, using total length instead of extension, and unit errors between millimetres and metres; and its permitted factual contextual examples are suspension spring wheel movement and a spring balance before permanent stretch.
- P20 has exact scope limited to charging by electron transfer, attraction and repulsion, sparks, earthing, and electric-field direction; its prerequisite diagnostic checks C1 atomic structure and the role and charge of electrons; its misconception checks target proton transfer, claiming every attraction proves opposite net charges, reversing conventional electric-field direction, and treating earthing as destruction of charge; and its permitted factual contextual examples are static on equipment with bonding and earthing around fuel handling and a rubbed plastic sheet attracting dust.

### Batch B: Foundational Mechanics

- P1 has exact scope limited to scalar and vector quantities, contact and non-contact forces, and free-body diagrams; its prerequisite diagnostic checks M2 unit conversion and consistent units, then establishes scalar and vector classification within P1; its misconception checks target drawing motion arrows as forces, merging weight with downforce or normal reaction, omitting force direction, and adding action-reaction partners to one body's diagram; and its permitted factual contextual examples are the five distinct straight-line F1 car forces named in the manifest and equal-size pushing and pulling of a stuck bolt.
- P2 has exact scope limited to `speed = distance / time`, average versus instantaneous speed, and distance-time graphs; its prerequisite diagnostic checks M2 conversion between kilometres, metres, hours, and seconds and M3 rearrangement; its misconception checks target confusing distance with displacement, average with maximum speed, graph height with speed, and gradient units; and its permitted factual contextual examples are the manifest Monza and Monaco average-speed comparison and fitting a merchants trip into glue working time.
- P3 has exact scope limited to `a = (v - u) / t`, velocity-time gradients and areas, and `v^2 = u^2 + 2as`; its prerequisite diagnostic checks P2 speed calculations, M6 gradient, and M7 area under a graph, then establishes the signed velocity and direction convention within P3; its misconception checks target treating velocity as speed, reporting negative acceleration inconsistently as positive deceleration, reading graph height as acceleration, and losing direction in area; and its permitted factual contextual examples are the manifest F1 acceleration and roughly `5g` braking figures and a cordless drill spinning up and coasting down.
- P4 has exact scope limited to stating and applying Newton's three laws, `F = ma`, and action-reaction pairs; its prerequisite diagnostic checks P1 force directions and free-body diagrams and P3 acceleration as a vector change, then establishes resultant force within P4; its misconception checks target believing motion requires a continuing resultant force, pairing balanced forces on one object as third-law partners, omitting mass units, and reversing force or acceleration direction; and its permitted factual contextual examples are tyre-track sideways interaction on a corner and nail-gun recoil with proper bracing.
- P5 has exact scope limited to `p = mv`, conservation of momentum, impulse, and force as rate of change of momentum; its prerequisite diagnostic checks P4 resultant force and Newton's laws; its misconception checks target conserving kinetic energy in every collision, dropping momentum signs, confusing force with impulse, and claiming a longer collision changes the required momentum change when initial and final states are fixed; and its permitted factual contextual examples are an F1 crash structure extending stopping time and a rubber mallet extending contact time compared with a steel hammer.
### Batch C: Energy, Moments, Fluids, Braking, And Drag

- P6 has exact scope limited to `W = Fd`, `P = E / t`, `P = Fv`, the joule, and the watt; its prerequisite diagnostic checks P4 force and motion direction and M3 substitution and rearrangement, then establishes that `Fd` uses displacement in the force direction; its misconception checks target using a non-parallel distance without qualification, confusing energy with power, treating watts as stored energy, and mixing joules, kilojoules, watts, and kilowatts; and its permitted factual contextual examples are the manifest 2026 power-unit figures and lifting a `25 kg` sand bag `3 m` up a ladder in different times.
- P7 has exact scope limited to energy stores and transfer pathways, conservation of energy, and efficiency; its prerequisite diagnostic checks M5 percentage calculations, then establishes the useful-input ratio within P7; its misconception checks target saying energy is used up, treating energy as a substance flowing without naming stores or pathways, allowing efficiency above 100 percent, and calling every output useful; and its permitted factual contextual examples are friction and regenerative braking and LED versus halogen lighting.
- P8 has exact scope limited to `moment = force x perpendicular distance`, the principle of moments, centre of mass, and stability; its prerequisite diagnostic checks P1 force direction and M4 proportional and scale reasoning; its misconception checks target using sloping distance instead of perpendicular distance, ignoring clockwise signs, assuming low centre of mass alone determines all vehicle load transfer, and confusing centre of mass with a pivot; and its permitted factual contextual examples are low F1 component packaging with the manifest suspension and roll-stiffness qualification, a long spanner, and a wheelbarrow.
- P9 has exact scope limited to `pressure = force / area`, `density = mass / volume`, and pressure in fluids and the atmosphere; its prerequisite diagnostic checks P1 normal force and M3 rearrangement; its misconception checks target confusing pressure with force, reversing the area relationship, assuming atmospheric pressure vanishes at altitude, and mixing density or pressure units; and its permitted factual contextual examples are tyre pressure changing contact-patch shape, thinner Mexico City air at `2200 m` reducing downforce and cooling, a bike pump, tank-water pressure, and a drawing pin.
- P31 has exact scope limited to thinking distance, braking distance, total stopping distance, and the effects of speed, reaction time, road conditions, and braking force; its prerequisite diagnostic checks P3 motion graphs and P7 kinetic-energy transfers; its misconception checks target treating thinking and braking distance as the same, assuming both scale linearly with speed, ignoring reaction time, and claiming one factor determines grip or stopping distance alone; and its permitted factual contextual examples are the listed F1 braking-marker factors and a loaded van on wet road versus an empty van on dry tarmac.
- P32 has exact scope limited to drag in fluids, changing acceleration, terminal velocity, and resultant-force reasoning; its prerequisite diagnostic checks P4 Newton's laws and P9 fluid ideas; its misconception checks target claiming drag is constant, claiming forces vanish at terminal velocity, confusing constant speed with no forces, and assuming acceleration falls because mass changes; and its permitted factual contextual examples are F1 straight-line acceleration falling as drag approaches driving force and flat versus crumpled paper.

### Batch D: Circuits, Magnetism, Mains, Grid, And Induction

- P10 has exact scope limited to `I = Q / t`, `V = IR`, circuit symbols, and current-voltage characteristics of components; its prerequisite diagnostic checks M3 substitution and rearrangement, then establishes the graph-axis convention before interpreting current-voltage characteristics; its misconception checks target current being consumed, potential difference flowing, resistance being fixed for every component, reversing graph axes without adjusting interpretation, and unit errors among amperes, coulombs, seconds, volts, and ohms; and its permitted factual contextual examples are the hybrid high-voltage warning light, damaged-chassis and marshal safety interpretation, and choosing cable for a light circuit from expected current without giving unsafe wiring instructions.
- P11 has exact scope limited to current, potential difference, and resistance in series and parallel circuits plus `P = VI` and `P = I^2R`; its prerequisite diagnostic checks P10 current, potential difference, resistance, and circuit symbols; its misconception checks target current splitting in series, voltage being identical across all series components, adding parallel resistance directly, total parallel resistance exceeding every branch, and confusing power with energy; and its permitted factual contextual examples are battery cells arranged for voltage or capacity and series versus parallel light strings.
- P12 has exact scope limited to magnetic fields, Fleming's left-hand rule, `F = BIl`, the motor effect, and electromagnetic induction; its prerequisite diagnostic checks P10 conventional current and circuit quantities; its misconception checks target confusing motor and generator effects, reversing current or field direction, applying the left-hand rule without three mutually perpendicular directions, and claiming a stationary magnet always induces current; and its permitted factual contextual examples are the MGU-K acting as generator and `350 kW` motor and a cordless drill motor versus a wind-up torch.
- P21 has exact scope limited to alternating mains supply, live, neutral, and earth wires, fuses, circuit breakers, and electrical power; its prerequisite diagnostic checks P10 current, potential difference, resistance, and circuit representation; its misconception checks target treating neutral or earth as always harmless, saying earth normally carries operating current, confusing a fuse with protection against every hazard, and mixing AC mains with the car's separate high-voltage system; and its permitted factual contextual examples are protected garage mains and separately isolated vehicle high voltage, plus an earthed or double-insulated metal-cased drill.
- P25 has exact scope limited to comparing renewable and non-renewable resources and explaining transformer-based reduction of transmission losses; its prerequisite diagnostic checks P7 energy transfers and efficiency and P11 electrical power and resistive heating; its misconception checks target calling renewable resources impact-free or continuously available, saying transformers create energy, and explaining high-voltage transmission without holding power fixed and relating lower current to cable loss; and its permitted factual contextual examples are electricity options for garages, timing, and broadcasting and rooftop solar with grid backup.
- P33 has exact scope limited to induction, transformer turns and power relationships, generators, and microphones; its prerequisite diagnostic checks P12 induction and P11 electrical power; its misconception checks target confusing primary with secondary quantities, assuming step-up voltage also steps up ideal power, ignoring alternating flux for a transformer, and treating generators or moving-coil microphones as motors; and its permitted factual contextual examples are generator recovery or production, circuit-facility distribution, a moving-coil microphone producing a signal by induction, and a mains adapter supplying safer low voltage.

### Batch E: Waves, Thermal Physics, Electromagnetic Spectrum, And Optics

- P13 has exact scope limited to `wave speed = frequency x wavelength`, transverse and longitudinal waves, reflection, absorption, and sound; its prerequisite diagnostic checks P2 speed, distance, time, and unit conversion; its misconception checks target particles travelling with the wave, sound travelling in a vacuum, confusing frequency with speed or amplitude, and mixing pulse echo time with one-way distance; and its permitted factual contextual examples are engine-note cues, team radio, wind-tunnel ultrasonic ride-height sensing, a stud finder, and an ultrasonic tape measure.
- P14 has exact scope limited to `E = mc x change in temperature`, specific latent heat, conduction, convection, and radiation; its prerequisite diagnostic checks P7 energy stores and transfer pathways; its misconception checks target confusing temperature with internal energy, applying the heat-capacity equation during a state change, describing convection in solids, and claiming metal must be at a lower temperature because it feels colder; and its permitted factual contextual examples are the manifest brake-disc and tyre working temperatures, the out-lap consequence of tyres starting below their working window, and equal-temperature metal and wooden door handles.
- P22 has exact scope limited to ordering electromagnetic waves, their common speed in vacuum, and uses and hazards across the spectrum; its prerequisite diagnostic checks P13 wavelength, frequency, and wave speed; its misconception checks target different vacuum speeds, reversing frequency and wavelength order, calling all electromagnetic radiation ionising, and claiming a use proves safety; and its permitted factual contextual examples are team radio, thermal cameras, visible inspection cameras, X-ray composite inspection, Wi-Fi, a TV remote, work lights, and a medical X-ray.
- P23 has exact scope limited to ray diagrams for reflection and refraction, angles measured from the normal, and speed changes at a boundary; its prerequisite diagnostic checks P13 wave speed and reflection, then establishes boundary speed changes within P23; its misconception checks target measuring from the surface, assuming every boundary bends a ray, reversing towards and away from the normal without considering relative speed, and treating a ray diagram as a literal light path with thickness; and its permitted factual contextual examples are braking markers appearing shifted or distorted through visor or camera-cover layers depending on layer shape and refractive index and an apparently displaced tile under water.
- P24 has exact scope limited to converging-lens ray diagrams, real and virtual images, colour filters, and total internal reflection; its prerequisite diagnostic checks P23 normals, refraction, and ray construction; its misconception checks target confusing real with upright, assuming a virtual image can be projected, claiming filters add colours, and omitting both critical-angle and direction conditions for total internal reflection; and its permitted factual contextual examples are on-board camera lenses, fibre-optic links, and a magnifying glass forming virtual or focused real images.
- P34 has exact scope limited to infrared emission and absorption by surfaces and interpretation of thermal-radiation practical data; its prerequisite diagnostic checks P14 thermal transfer and P22 infrared within the electromagnetic spectrum; its misconception checks target claiming dull black surfaces only emit but do not absorb well, ignoring temperature and area controls, confusing infrared images with direct temperature measurements, and proposing unsafe handling of hot cans; and its permitted factual contextual examples are thermal imaging of tyres and brakes, component surface finish, a dull black radiator, and the four identical hot-can comparison.

### Batch F: Radiation, Space, Stars, Cosmology, And Nuclear Physics

- P15 has exact scope limited to alpha, beta, and gamma radiation, half-life, penetration, uses, and hazards of ionising radiation; its prerequisite diagnostic checks C1 atomic structure, nuclei, and isotopes; its misconception checks target treating alpha, beta, and gamma as identical electromagnetic waves, equating penetration with ionising ability, claiming radiation remains in every irradiated object, and giving unsafe source-handling advice; and its permitted factual contextual examples are X-ray or CT inspection with the manifest qualification that X-rays are not alpha, beta, or gamma emissions, plus a smoke alarm and dental X-ray.
- P26 has exact scope limited to the Solar System, planets, moons, artificial satellites, gravity, centripetal force, and circular-orbit acceleration; its prerequisite diagnostic checks P1 vector force and M4 proportional reasoning; its misconception checks target an outward balancing force in the inertial frame, no acceleration at constant speed, gravity disappearing in orbit, and confusing speed with velocity; and its permitted factual contextual examples are satellites supporting timing, mapping, and logistics and satellite-navigation signals from orbiting clocks.
- P27 has exact scope limited to star formation, fusion as the stellar energy source, and stellar evolution according to initial mass; its prerequisite diagnostic checks P26 the Solar System and gravity and P6 energy and power; its misconception checks target stars burning chemically, every star following one life cycle, treating fusion as chemical combustion, and implying all elements form in one identical stellar stage; and its permitted factual contextual examples are carbon, aluminium, and iron in a racing car and iron atoms in steel tools originating through stellar processes.
- P28 has exact scope limited to red-shift as evidence for expansion and cosmic microwave background radiation as evidence related to the Big Bang model; its prerequisite diagnostic checks P22 electromagnetic wavelength and frequency and P27 stellar context, then establishes galaxies as the observation scale within P28; its misconception checks target treating red-shift as visible redness, using one shifted object as complete proof, locating an explosion centre in ordinary space, and presenting a scientific model as unqualified literal reality; and its permitted factual contextual examples are laboratory or materials spectrometers and a workshop spectroscope compared with astronomical spectral-line measurements.
- P30 has exact scope limited to half-life calculations, irradiation versus contamination, and evaluation of medical uses of radioactive sources; its prerequisite diagnostic checks P15 radiation properties and half-life and M19 relationship and graph interpretation, then establishes repeated halving without assuming prior exponential mathematics; its misconception checks target linear subtraction each half-life, ignoring background count, confusing activity with remaining mass, conflating irradiation and contamination, and recommending medical exposure; and its permitted factual contextual examples are a motorsport medical centre using short-lived hospital tracers and a smoke alarm whose sealed casing helps prevent contamination while radiation ionises air.
- P29 has exact scope limited to comparing nuclear fission and fusion, fission chain reactions, and the extreme temperature and pressure needed for fusion; its prerequisite diagnostic checks P30 half-life and radioactive-source context, then establishes neutron-induced fission and chain reactions within P29; its misconception checks target confusing fission with radioactive decay, saying a neutron chain reaction is fusion, claiming ordinary F1 power units use fusion, and making unsupported claims about grid generation or reactor safety; and its permitted factual contextual examples are possible fission contribution to national grids, fusion powering the Sun rather than an F1 power unit, and household electricity potentially including controlled fission generation.

### Batch G: Cross-Cutting Practical Planning

- P35 has exact scope limited to identifying variables, choosing suitable instruments, repeating measurements, assessing uncertainty, and evaluating a physical method; its prerequisite diagnostic checks M13 justified precision and M16 tables, axes, variables, scales, and units, then establishes any needed distinction among repetition, uncertainty, repeatability, and reproducibility within P35 rather than inventing an M20 prerequisite; its misconception checks target confusing control variables with a control group, calling repetition reproducibility, quoting instrument resolution without relating it to uncertainty, changing several factors at once, and proposing unsafe apparatus; and its permitted factual contextual examples are an F1 correlation test controlling fuel, tyres, and conditions and a fair insulation comparison using equal containers, equal starting temperatures, fixed times, and repeated readings.

## Drafting Workflow

1. Record `git status --short`, a catalog checksum, and path-specific checksums for all existing `topics/maths/` files before creating drafts.
2. Record a normalized snapshot of the existing M1-M20 catalog entries in the work-area `README.md` so their keys, values, and order can be compared after the intentional catalog edit.
3. For a fresh start, create `/var/folders/f8/bb6ngcg16k5b6sd83h72xy880000gn/T/opencode/gcse-physics-resource-drafts/` as the unpublished work area under the approved persistent temporary root, and record a short `README.md` there that identifies the current batch, completed topic gates, unresolved evidence gaps, and exact next action.
4. For a resumed run, read the existing `README.md`, inventory P1-P35 work directories, verify existing drafts rather than overwriting them, and scaffold only missing topic directories.
5. Run the project-local `scaffold-topic.mjs` with explicit HTML, resource root, topic ID, and a unique output directory under that disclosed work area, which is outside the published tree.
6. Confirm each scaffolder result reports subject `physics`, the lowercase `p1` through `p35` directory, and the exact catalog destination listed below.
7. Keep every generated `draft: true` marker and every `EVIDENCE_REQUIRED` placeholder while research, writing, or independent review remains incomplete.
8. Write each summary as concise revision prose that defines the topic-specific quantities and models, states relevant relationships with units and sign conventions, gives only evidence-supported qualifications, and addresses the named misconceptions without copying the manifest specification.
9. Write each tutor prompt as a genuinely interactive session that checks the exact prerequisites above, teaches in short chunks, pauses for an answer, adapts to the answer, uses only the permitted factual contexts or neutral abstract questions, probes misconceptions, checks retrieval, and ends with GCSE-style practice, marking, and actionable feedback.
10. Permit the tutor to invent simple fictional numerical values, neutral diagrams, abstract objects, and fictional datasets needed for varied questions, but prohibit invented physical relationships, factual context, practical claims, motorsport details, or additional real-world examples.
11. Require the tutor prompt to ask for a learner attempt before revealing solutions and to refuse unsafe practical instructions and board-specific overclaims.
12. Require calculations to expose any needed compatible-unit conversions, vector directions, sign conventions, rearrangement, substitution, and a final dimensional and physical-sense check rather than presenting unexplained numerical answers.
13. Require model-based explanations to qualify a representation where literal interpretation would create a named misconception, without adding repetitive boilerplate to every diagram or calculation.
14. Update the work-area `README.md` after every research or review handoff so the task cursor plus work-area status is sufficient to resume after session loss.
15. Remove every placeholder and deliberately remove `draft: true` only after an independent execution has approved the complete topic, every selected final destination, exact publication path, and metadata.
16. After all 35 topics pass independently, re-read the current catalog and stop if any P1-P35 destination directory or mapping now exists, any M1-M20 mapping differs from the baseline, or concurrent changes make the planned merge stale.
17. Build and review one `apply_patch` publication transaction containing all 105 `Add File` sections and the catalog edit in P1-P35 order against the current catalog, rather than copying files and editing the catalog as separate operations.
18. If publication reports a failure or the session stops unexpectedly, inventory every destination before retrying; accept an already-created file only when it is byte-for-byte identical to its approved draft, stop on any mismatch or unexpected mapping, and construct a new patch containing only verified missing files plus the still-required catalog edit.
19. Never overwrite, regenerate, delete, or reconstruct an existing destination from the initial snapshot during recovery.
20. Copy the final approved evidence logs into `prototype.tests/gcse-science-f1-roadmap/support-resources.task.evidence/physics/` as durable task artifacts, including the selected destinations and independent verdicts, while keeping generated topic drafts out of the repository.
21. After publication evidence is recorded in the task and durable evidence files, remove the temporary draft area so generated drafts do not remain as stale parallel sources.

## Link Research And Review

Search for the exact physical concept and required learner action rather than broad phrases such as GCSE physics revision.
Prefer direct lesson, practice, interactive, video, reference, or official educational pages from public institutions, universities, established educational publishers, and reputable open projects.
Open every candidate in a fresh browser context, follow every redirect, and inspect the final destination rather than relying on search snippets, HTTP status, or audit output.
Require at least two distinct supporting URLs per topic, and do not add weaker links merely to increase the count.
Both supporting resources must use direct HTTPS final URLs, be credible, free, useful without payment, account, trial, app installation, or hidden content gate, and directly explain or practise the topic's exact scope.
Prefer complementary support, such as one clear conceptual lesson and one focused practice or interactive resource, while never accepting lower quality merely for publisher diversity.
Reject generic home pages, search pages, broad indexes that do not directly expose clearly labelled exact-topic material, misleading redirects, unqualified unsafe practicals, inaccessible documents, and pages whose useful material contradicts or misleadingly reframes the manifest scope.
Allow a strong destination to contain incidental adjacent material when its selected section directly supports the topic and the label and note do not overstate coverage.
Choose one separate focused English Wikipedia article per topic, use an `https://en.wikipedia.org/wiki/...` final URL, and reject portals, disambiguation pages, forced near-matches, and articles whose title or scope would make the label misleading.
Where one manifest topic combines concepts, select the Wikipedia article that most directly anchors the central concept and record uncovered components as a review limitation rather than overstating article coverage.
Record the final page's accurate title as the label, its actual publisher, the closest allowed kind, and a concrete note describing exactly what the learner can understand or practise at that destination.
Stop publication when no honest pair of supporting resources or focused Wikipedia article can be established.
Run the network audit only after manual final-destination inspection, and treat redirects, status codes, content types, and likely-gate signals as prompts for reinspection rather than evidence of educational quality.

## Exact Publication Files And Catalog Mappings

The product publication patch edits only `prototypes/gcse-science-f1-roadmap/catalog.json` and the 105 new physics files identified below.
Orchestration bookkeeping separately edits the managed task file, this plan, and durable evidence Markdown under `prototype.tests/gcse-science-f1-roadmap/support-resources.task.evidence/physics/`.
Keep `catalog.json` at `schemaVersion: 1` and preserve all M1-M20 keys, values, and ordering exactly while appending the physics mappings in ID order.

- P1 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p1/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p1/topic.json`.
- P2 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p2/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p2/topic.json`.
- P3 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p3/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p3/topic.json`.
- P4 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p4/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p4/topic.json`.
- P5 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p5/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p5/topic.json`.
- P6 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p6/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p6/topic.json`.
- P7 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p7/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p7/topic.json`.
- P8 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p8/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p8/topic.json`.
- P9 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p9/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p9/topic.json`.
- P10 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p10/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p10/topic.json`.
- P11 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p11/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p11/topic.json`.
- P12 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p12/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p12/topic.json`.
- P13 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p13/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p13/topic.json`.
- P14 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p14/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p14/topic.json`.
- P15 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p15/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p15/topic.json`.
- P16 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p16/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p16/topic.json`.
- P17 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p17/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p17/topic.json`.
- P18 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p18/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p18/topic.json`.
- P19 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p19/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p19/topic.json`.
- P20 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p20/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p20/topic.json`.
- P21 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p21/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p21/topic.json`.
- P22 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p22/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p22/topic.json`.
- P23 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p23/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p23/topic.json`.
- P24 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p24/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p24/topic.json`.
- P25 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p25/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p25/topic.json`.
- P26 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p26/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p26/topic.json`.
- P27 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p27/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p27/topic.json`.
- P28 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p28/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p28/topic.json`.
- P29 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p29/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p29/topic.json`.
- P30 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p30/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p30/topic.json`.
- P31 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p31/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p31/topic.json`.
- P32 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p32/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p32/topic.json`.
- P33 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p33/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p33/topic.json`.
- P34 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p34/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p34/topic.json`.
- P35 publishes `prototypes/gcse-science-f1-roadmap/topics/physics/p35/{topic.json,summary.md,explain-prompt.md}` and maps to `topics/physics/p35/topic.json`.

## Preservation Gates

Confirm the embedded P1-P35 records and complete prerequisite DAG are byte-for-byte unchanged after publication.
Confirm all M1-M20 directories and their `catalog.json` mappings are byte-for-byte unchanged.
Confirm `prototypes/gcse-science-f1-roadmap/papers.json` remains exactly the valid empty independent graph with `schemaVersion: 1`, no nodes, and no edges.
Confirm `prototypes/gcse-science-f1-roadmap.html`, runtime behavior, validators, link auditor, scaffolder, package scripts, fixtures, and existing tests have no slice diff.
Reject any topic metadata that duplicates manifest title, specification, prerequisites, check, F1, DIY, stage, driver, or subject fields.
Reject orphan directories, draft markers, placeholders, unexpected files, unlisted catalog entries, and prerequisite-like fields in resource records.

## Review Gates

### Per-Topic Gate

The independent reviewer must confirm exact manifest scope, all exact prerequisite IDs, the topic-specific prerequisite diagnostic, the named misconceptions, and exclusive use of permitted factual contexts while allowing neutral abstract questions.
The independent reviewer must verify physics accuracy, board neutrality, model qualifications, signs and directions, unit consistency, dimensional sense, and absence of invented engineering or curriculum claims.
The independent reviewer must verify that practical language is safe, observational, and suitable for supervised GCSE learning rather than actionable hazardous advice.
Nail-gun recoil, sealed aerosol heating, fuel-area bonding, mains and high-voltage systems, focused sunlight, ionising sources, medical exposure, and hot-can contexts may be explained or analysed but never converted into learner-executed procedures.
The independent reviewer must run the prompt mentally and confirm that it waits for answers, adapts, checks retrieval and misconceptions, withholds solutions until an attempt, and finishes with marked exam-style practice and feedback.
The independent reviewer must inspect every declared supporting final destination and the focused English Wikipedia final destination, then confirm accurate labels, publishers, kinds, notes, HTTPS URLs, direct relevance, free access, and no account or payment gate.
The topic may lose `draft: true` only after all parts of this gate pass without self-approval.

### Batch Gate

After each bounded batch, construct a disposable validation library under the approved temporary root from the current published library plus that batch's reviewed publishable drafts and temporary catalog mappings.
Run the real manifest and resource validators against that complete disposable library, then remove it; do not validate isolated topic files or publish the batch early.
Audit every selected batch destination and manually reinspect every redirect, likely gate, unexpected content type, failed status, or network error.
Review the approved batch drafts and evidence logs, allowing only the intended topic directories and metadata while the final product tree remains unchanged.
Recheck cross-batch prerequisites from the manifest without changing publication readiness or adding resource-level dependency fields.

### Subject Gate

Require exactly 35 cataloged physics IDs matching P1 through P35, with one exact catalog path and exactly three files for every physics directory.
Require all 35 topics to pass independent topic review and all seven batches to pass their batch gates.
Require at least 105 manually inspected physics URLs in total, consisting of at least 70 supporting resources and 35 English Wikipedia destinations, with no unresolved failure or likely gate.
Because the audit script scans the full catalog, the final run should report at least 165 successful destinations across the preserved 60 maths URLs and the new physics URLs.
Require the manifest validator to continue reporting 121 nodes, 189 prerequisite edges, and exactly 35 physics nodes.
Require the resource validator to report 55 published topics, zero papers, and zero paper edges at this slice boundary.

## Browser Acceptance

Use the existing local HTTP Playwright server and locally routed Jelly UI, Open Props, and Marked fixtures instead of live CDN dependencies.
Open P16 for particle models, P3 for signed motion graphs, P5 for momentum, P11 for circuits, P12 for motor and generator distinctions, P14 for thermal physics, P24 for optics, P30 for radiation, P28 for cosmology, and P35 for practical planning.
For every representative, require support state `ready`, rendered summary text without raw Markdown, every declared supporting link plus the one declared Wikipedia link, clipboard text exactly equal to the full `explain-prompt.md` bytes, and an independently loaded empty-paper state.
At viewport widths 320, 390, 768, and 1360 pixels, inspect one short bundle and one content-dense bundle and require zero page-level horizontal overflow, readable wrapped links, reachable copy controls, and a usable drawer.
Block Marked for one content-dense physics representative and require safe preview failure while the unrendered full prompt remains exactly copyable.
Delay the resource response for one physics topic, rapidly switch to another physics topic, and require the stale response not to replace the active drawer.
Independently delay or fail `papers.json` and require valid physics summaries, prompts, links, and copy controls to remain available while the paper section alone reports empty or unavailable state as appropriate.
Treat page errors, failed local resource requests, support-state errors, clipboard mismatches, incorrect hrefs, stale replacement, overflow, inaccessible controls, or coupling to paper state as acceptance failures.

## Commands From Repository Root

On a clean checkout, install the isolated locked test dependency once before validation.
Fetch browser fixtures once only when any required fixture is missing.
For a fresh draft run, create the unpublished scaffold area only after confirming the approved temporary root is the intended parent and the destination does not already exist.
For a resumed draft run, do not recreate or overwrite the area; inspect its README and scaffold only missing topic directories.

```sh
ls /var/folders/f8/bb6ngcg16k5b6sd83h72xy880000gn/T/opencode
mkdir /var/folders/f8/bb6ngcg16k5b6sd83h72xy880000gn/T/opencode/gcse-physics-resource-drafts
for number in $(seq 1 35); do output="/var/folders/f8/bb6ngcg16k5b6sd83h72xy880000gn/T/opencode/gcse-physics-resource-drafts/p${number}"; if [ ! -e "$output" ]; then node prototype.tests/gcse-science-f1-roadmap/skills/gcse-roadmap-resources/scripts/scaffold-topic.mjs --html prototypes/gcse-science-f1-roadmap.html --resources prototypes/gcse-science-f1-roadmap --topic "P${number}" --output "$output" || exit 1; fi; done
npm ci --prefix prototype.tests/gcse-science-f1-roadmap
```

Run these checks after each batch where practical and again after all 35 topics are published.

```sh
node prototype.tests/gcse-science-f1-roadmap/tests/validate-manifest.mjs prototypes/gcse-science-f1-roadmap.html
npm run validate --prefix prototype.tests/gcse-science-f1-roadmap
node prototype.tests/gcse-science-f1-roadmap/tests/audit-resource-links.mjs prototypes/gcse-science-f1-roadmap
npm test --prefix prototype.tests/gcse-science-f1-roadmap
git diff --check
git status --short
```

Run `prototype.tests/gcse-science-f1-roadmap/tests/fetch-fixtures.sh` once only if `jelly.js`, `open-props.css`, or `marked.js` is missing from the ignored fixture directory.
Capture the final audit output outside the repository, count its JSON records, reconcile every record to the current catalog, and summarize the timestamp, exact count, failures, redirects, and gate signals in the managed task and durable evidence artifact.
Use `git diff HEAD --` with the protected paths for `prototypes/gcse-science-f1-roadmap.html`, `prototypes/gcse-science-f1-roadmap/papers.json`, `prototypes/gcse-science-f1-roadmap/topics/maths`, validators, runtime tests, and package files so both staged and unstaged protected changes are visible; exclude the managed task, plan, and evidence paths that orchestration intentionally updates.
Before final product review, stage only `catalog.json` and the intended P1-P35 files, verify an exact inventory of 105 new files and 35 mappings, and use `git diff --cached -- prototypes/gcse-science-f1-roadmap/catalog.json prototypes/gcse-science-f1-roadmap/topics/physics` so untracked additions cannot escape review.
Browser acceptance may use a temporary Playwright specification under the approved temporary directory or the existing harness without committing or editing test files, and the exact invocation and observed representatives must be recorded as evidence during the later acceptance step.
Remove temporary acceptance specifications, downloads, audit captures, `test-results`, and `playwright-report` after recording evidence, and confirm none is staged or committed.

## Tradeoffs And Risks

Coherent domain batches improve consistency and expose cross-topic contradictions, and per-topic review remains independent even though final product publication is subject-atomic to preserve catalog ordering and recoverability.
Two supporting resources is the minimum rather than a target for padding, so a third is retained only when it adds distinct high-quality value and every declared destination passes the same review.
The audit can establish connectivity and flag obvious gates but cannot prove physics accuracy, safe advice, educational quality, model qualification, or true ungated access, so independent manual inspection remains mandatory.
Narrow or compound physics topics may lack one Wikipedia article covering every component, so a focused central article is acceptable only with an accurate label and no claim of complete topic coverage.
Some credible education sites vary content by region, cookies, scripts, or bot detection, so final browser inspection must use the learner-visible destination and unresolved access ambiguity must block the topic.
F1 examples are engaging but especially prone to changing regulations, approximate figures, and hidden engineering assumptions, so only the manifest wording may be used unless separately inspected evidence supports a restrained explanation within scope.
Physics notation differs across sources, especially current-voltage graph axes, sign conventions, transformer assumptions, group numbering inherited from chemistry, and temperature notation, so every bundle must state its convention rather than silently merge incompatible presentations.
Hazardous domains create a risk that otherwise accurate prose becomes operational advice, so mains, high-voltage, fuel, pressure, heat, radiation, laser, and medical examples must remain explanatory and defer practical execution to approved supervised methods.
External destinations can change after review, so preserve the evidence logs and perform the 105-URL final inspection and full-catalog audit as close to publication as practical.
