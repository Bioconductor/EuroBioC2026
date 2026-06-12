.gallery_header <- function() {

    cat('
<link rel="stylesheet"
href="https://cdn.jsdelivr.net/npm/glightbox/dist/css/glightbox.min.css">

<script src="https://cdn.jsdelivr.net/npm/glightbox/dist/js/glightbox.min.js"></script>

<style>

.gallery{
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
    gap: 4px;
}

.gallery-item{
    display: block;
}

.gallery img{
    width: 100%;
    height: 100px;
    object-fit: cover;
    border-radius: 4px;
}

.gallery img:hover{
    transform:scale(1.03);
}

</style>

<div class="gallery">
')
}

.gallery_photo <- function(photo_path, alt = basename(photo_path)) {

  photo_path <- gsub("\\\\", "/", photo_path)
  file_name <- tools::file_path_sans_ext(basename(photo_path))

  cat(sprintf(
    '<div class="gallery-item"><a href="%s" class="glightbox" data-gallery="conference" title="%s"><img src="%s" loading="lazy" alt="%s"></a></div>',
    photo_path,
    file_name,
    photo_path,
    alt
  ))
}

.gallery_footer <- function() {

    cat('
</div>

<style>

.gdownload{
    position:absolute;
    top:15px;
    right:60px;

    width:40px;
    height:40px;

    display:flex;
    align-items:center;
    justify-content:center;

    border-radius:50%;
    background:rgba(0,0,0,.65);

    color:white;
    text-decoration:none;
    font-size:22px;
    z-index:9999;
}

.gdownload:hover{
    background:rgba(0,0,0,.85);
}

</style>

<script>

const lightbox = GLightbox({
    selector: ".glightbox",
    touchNavigation: true,
    loop: true,
    zoomable: true,
    captions: true,
    captionPosition: "bottom"
});

lightbox.on("slide_after_load", ({ slideNode, slideConfig }) => {

    slideNode.querySelector(".gdownload")?.remove();

    const button = document.createElement("a");

    button.className = "gdownload";
    button.href = slideConfig.href;
    button.download = "";
    button.title = "Download photo";
    button.innerHTML = `
<svg width="22" height="22" viewBox="0 0 24 24"
     fill="none" stroke="white" stroke-width="2"
     stroke-linecap="round" stroke-linejoin="round">
  <path d="M12 3v12"/>
  <path d="M7 10l5 5 5-5"/>
  <path d="M5 21h14"/>
</svg>`;

    slideNode.appendChild(button);

});

</script>
')

}

.create_photo_gallery <- function(
        dir_path = file.path("..", "images", "conference_photos")
) {

    photos <- sort(list.files(
        dir_path,
        pattern = "\\.jpe?g$",
        ignore.case = TRUE,
        full.names = TRUE
    ))

    .gallery_header()

    for (photo in photos) {
        .gallery_photo(photo)
    }

    .gallery_footer()

}
