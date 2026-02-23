# GDScript

## Özet
GDScript; 2014 yılında gelişmeye başlayan, ücretsiz ve Açık Kaynaklı (Open Source) **Godot Oyun Motoru** için özel olarak (Juan Linietsky tarafından) icat edilen, yazım şekli (Sintaksı) Python'a inanılmaz derecede benzemesine rağmen C++ motoruyla doğrudan bütünleşik çalışan yüksek seviyeli, Nesneye-Yönelik (Object-Oriented) bir **Oyun Betik (Game Scripting) Dilidir**.

## Nedir ve Ne İşe Yarar?
Oyun motorları dünyasında Unity C# kullanır, Unreal Engine ise C++ (ve Blueprints). Godot oyun motoru ilk çıktığında geliştiriciler Python, Lua veya Squirrel dillerini motora entegre etmeye çalıştı. Lakin Godot'nun mimarisi (Düğüm/Node Sistemi) o kadar kendine hastı ki, Dışarıdan gelen hiçbir dil bu mimariyle pürüzsüz çalışmıyor, performans kaybediyor ve Çöp Toplayıcıları (Garbage Collector) oyunu anlık donduruyordu.

Juan Linietsky dediki: **"Neden Motora %100 Entegre, Python gibi kolay yazılan ama C++ içyapısına doğrudan kancalanan Kendi Dilimizi Yazmıyoruz?"**
GDScript böyle doğdu. O kadar optimize edilmiştir ki, Godot Ekranında bir karakteri sağa kaydırmak istediğinizde başka hiçbir dil bunu GDScript kadar Cihazı kasmadan ve az satırla yapamaz.

**Ne İşe Yarar?**
* **Bağımsız (Indie) Oyun Geliştirme:** Sonic Colors: Ultimate, Brotato, Cassette Beasts gibi milyonlar satan bağımsız oyunların arkasındaki yapay zeka, karakter hareketleri ve kullanıcı arayüzü (UI) tamamen GDScript ile kodlanmıştır.
* **Hızlı Prototipleme:** Python'a benzediği için Noktalı virgül (;) veya Süslü parantez ({}) yoktur. Bir oyun mekaniğini (Örn: Çift zıplama) Unity'de C# ile 30 satırda yazarken, GDScript ile 8 satırda yazar ve anında test edersiniz.

## Dilin Mantığı ve Kod Yapısı
Tamamen Python'un İkiz kardeşidir (Boşluklara/Girintilere duyarlıdır). Ancak Python'dan farklı olarak; C# veya C++'taki gibi "Statik Yazım (Static Typing)" özelliğini (İsteğe bağlı olarak) barındırır.
Godot'nun temeli olan **Düğüm (Node)** hiyerarşisiyle yaşar. Her betik(script) bir Düğüm'e (Örn: Oyuncu, Kamera, Kılıç) yapıştırılır.

**Önemli Fonksiyonları:**
* `_ready()`: Oyun (veya Düğüm) ekranda doğduğu (İlk saniye) an Sadece 1 Kere tetiklenir (Kurulumlar).
* `_process(delta)`: Oyun çalıştığı sürece Saniyede 60 Kere (Her Frame) tetiklenir. Karakterin sürekli Yürümesi/Düşmesi buraya yazılır.

### Örnek Bir GDScript Kodu: Bir Karakteri Yürütmek ve Zıplatmak
Godot oyun motorunda "Oyuncu (KinematicBody2D)" karakterinin içine atılmış klasik bir 2 Boyutlu (Platformer - Mario) yürüme Kodu:

```gdscript
# BU BIR GDSCRIPT KODUDUR (.gd uzantili) - Python'a asiri benzer!

extends KinematicBody2D # Bu Kod, Fiziksel Oyuncu Objesini(C++) Genisletir!

# Degisken (Variables) Tanimlamalari - `var` veya `export` kullanilir.
# export yazarsaniz, Bu degisken Godot'un Gorsel Arayuzunde (Menu) Gorunur!
export var yurume_hizi = 200     # Saga/Sola Gitme Hizi
export var ziplama_gucu = -400   # Yukari Ziplama (- Y ekseni yukari ceker)
export var yer_cekimi = 1000     # Asagi Dusme Fiziği

var yon_vektoru = Vector2.ZERO   # X ve Y yonlerindeki Guncel Ivmemiz(Hızımız)

# Saniyede 60 Kare(Frame) Calisan OYUN DONGUSU(Physics Process):
func _physics_process(delta):
    
    # Kendi Ivmemizi (Y Eksenini) Yer Cekimine Göre Asagi(Artı) Cek! Puan duselim
    yon_vektoru.y += yer_cekimi * delta

    # Sag/Sol Tusuna Basma Kontrolu (Mantikli Input Algilamasi)
    yon_vektoru.x = 0
    
    if Input.is_action_pressed("ui_right"):
        yon_vektoru.x = yurume_hizi
    elif Input.is_action_pressed("ui_left"):
        yon_vektoru.x = -yurume_hizi

    # ZIPLAMA: Eger Karakter Yere Degiyorsa (is_on_floor) ve BOSLUK (ui_up) Basildiysa:
    if is_on_floor() and Input.is_action_just_pressed("ui_up"):
        yon_vektoru.y = ziplama_gucu

    # GODOT FIZIK MOTORUNU TETIKLE: Karakteri Farki Hesaplayarak Hareket Ettir! 
    # (Duvara çarptı mı diye Kendi Fizik motoru cözer!)
    yon_vektoru = move_and_slide(yon_vektoru, Vector2.UP)
```

Görüldüğü gibi kodun okunabilirliği mükemmeldir. Unity'deki `GetComponent<Rigidbody2D>().velocity` gibi karmaşık uzun metinlerden arınmış; Oyun motorunun içine *Şırınga ile enjekte edilmiş* kadar uyumludur.

## Kimler Kullanır?
* Evrendeki bütün **Godot Oyun Motoru (Indie) Geliştiricileri**.
* Özellikle Unity'nin 2023 yılında "İndirme Başına Ücret (Runtime Fee)" politikasının fiyaskoyla sonuçlanmasının ardından, C# yazılımcılarının büyük bir protestoyla Godot Engine'e geçmesi sonucu, GDScript son yıllarda Oyun Ekosisteminin En çok Geliştirici Çeken Altın Çocuğu Olmuştur. Açık kaynak camiasının Kalbini çalan Bir Şaheserdir.
