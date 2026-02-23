# GML (GameMaker Language)

## Özet
GML (GameMaker Language); Yoyo Games tarafından üretilen, Dünyanın en köklü ve En Başarılı Bağımsız (Indie) 2D oyun Motorlarından biri olan **GameMaker Studio**'nun arkasında çalışan, C, JavaScript ve Pascal karışımı esnek bir yapıya sahip olan; Oyun Objelerinin (Objects) olaylarını (Events) tetiklemek için tasarlanmış Çok hızlı ve Öğrenmesi Mükkemmel Derecede kolay bir **Oyun Betik Dili'dir.**

## Nedir ve Ne İşe Yarar?
1999 yılında Mark Overmars, "Çocuklar bile oyun yapabilsin" diyerek GameMaker programını ürettiğinde, insanlar oyunlarını "Sürükle-Bırak (Drag&Drop)" tuşlarıyla yapıyordu. Ancak oyunlar Karmaşıklaştıkça (Suni zeka, Matematik, Envanter sistemleri eklendikçe) sürükle-bırak yetmedi.

Sürükle Bırak tuşlarının Arkaplandaki "Gerçek Metin / Yazılım Dünyası" olarak GML Piyasaya Çıktı!
GML, Katı kuralcı diller (C++ veya Java) gibi adamı Sıkboğaz etmez. Bir değişken yaratmak için Cihazın Belleğini Deşmezsiniz. Dümdüz `can = 100` yazıp geçersiniz. Bu Yüzden Mükemmeldir.

**Ne İşe Yarar (Kimler Meydana Geldi)?**
Eğer PC veya konsol geçmişinize bakarsanız Oynadığınız o Trilyon Satan Bağımsız Pikselli Şaheserlerin Çoğu GML ile kodlanmıştır:
* **Undertale:** (Toby Fox, Neredeyse Sıfır programlama tecrübesiyle Cihazı GML ile kodlayıp dünyayı salladı).
* **Hotline Miami:** (O Işık Hızındaki Katliam Optimizasyonu GML Motoruyla koşar).
* **Katana ZERO, Forager, Hyper Light Drifter, Nuclear Throne...** Bunların Hepsinin Atardamarı GameMaker Language'tir.

## Dilin Mantığı ve Kod Yapısı
GameMaker'ın Mimarisi **Objeler (Objects)** ve **Zamanlar (Events)** üzerinedir. 
Siz Kod Dosyasının Tepesine Değil; "Karakteriniz Oluştuğunda (Create Event)" kutucuğuna ayrı kod, "Karakter Her Saniye Yürürken (Step Event)" kutucuğuna ayrı GML Kodu yazarsınız.

Yazım şekli (Syntax) C ve Javascript harmanıdır. Noktalı Virgül (;) ve Süslü Paramtez `{}` kullanabilirsiniz lakin GML bazen Unutursanız bile o Kadar Esnektir(Forgiving) Ki Programı "Hata!" diye çökertmez sizi affeder çalıştırır (Eski Sürümlerinde çok barizdi, Şimdi biraz Daha derleyicisi Katılaştı).

### Örnek Bir GML Kodu: Bir Uzay Gemisini Fareye Doğru Döndürmek ve Ateş Etmek
Bir `obj_Gemi` isimli Oyun nesnesinin "Her Saniye(Frame) tetiklenen Update/Step" dosyasında calisan; Gemiyi imlece cevirip Sol Tıkla mermi Yaratan Basit Kodu:

```gml
/* BU BIR GML (GameMaker Language) STEP EVENT KODUDUR */

// 1. GEMININ YUZUNU(ACISINI) MAUSE İMECİNE DOĞRU ÇEVİRME:
// point_direction(x1,y1, x2,y2) Özel bir GML Matematik Fonksiyonudur:
var fareyeCevir_Acisi = point_direction(x, y, mouse_x, mouse_y);

// image_angle -> Objenin ekranda cizilen Görüntü Açısını değiştirir
image_angle = fareyeCevir_Acisi;


// 2. TIKLANMA KONTROLU (Eğer Farenin Sol Tuşuna(Mb_Left) Basılırsa)
if (mouse_check_button_pressed(mb_left)) {
   
   // 3. MERMI OLUSTUR ("obj_mermi" ismindeki baska nesneyi Gemnin burnun ucunda Yarat!)
   // (instance_create_layer Cok Klasik modern GameMaker Cihazidir)
   var yeni_mermi = instance_create_layer(x, y, "Mermiler_Katmani", obj_mermi);
   
   // MERMININ HIZINI VE YONUNU (Geminin yuzunu dondugu Yone Dogru) FIKSELE!
   // 'with' anahtari Muazzam bir GML ozelligidr (Secilen Diger objenin Icine Girip onun degisknelerini editlersin!)
   with (yeni_mermi) {
       speed = 15;                       // Mermiye Atesleme hizi ver!
       direction = other.image_angle;    // (other. = Mermiyi Yaratanın/Geminin kendi acisi) Merminin gidis Yönüne eşitle
       image_angle = direction;          // Merminin Görseli de o yone donsun
   }
   
   // SES CAL (Ates Etme Efekti)
   audio_play_sound(snd_AtesEdildi, 1, false);
}
```

Bu kadar kısa bir kod, "Fareyi takip eden, Basınca Silah sıkan ve Ses çıkaran" Tıklanabilir (Hotline Miami vari) Bir Uzay Gemisini Ekrana pürüzsüz Cizer! `with` blokları gibi yapılar, Başka yazılım dillerinde Pointerlarla/Refernasla Cebelleşeceğiniz yerlerde İnanılmaz "Tatlı" bir Geliştirici Deneymi sunar.

## Kimler Kullanır?
* Ekibi olmayan, Tek başına(Solo) Yıllarca Odasına Kapanıp Milyon dolarlık Hayalinin Arkasından Giden **Solo Indie Oyun Geliştiricileri**.
* Özellikle Yeni Başlayan Lise/Üniversite öğrencileri; C++ ve Unity C#'ın Devasa Class hiyerarşilerinde Boğulmadan, Direkt Ekrana Odaklanıp "Oyunun Eğlence Mantığnı" Kodlamak istediklerinde, Dünyadaki 2D Motorların Tartışmasız Babası Olan GameMaker'a Ve Onun Can suyu GML'ye Sarılırlar. Psikolojik Porselen olarak Çok Esnek ve Başarlıdır.
