# OpenGL

## Özet
OpenGL (Open Graphics Library); 1992 yılında Silicon Graphics (SGI) tarafından piyasaya sürülen, bilgisayarın Ekran Kartı (GPU) ile doğrudan ve Evrensel (İşletim sisteminden bağımsız) olarak iletişim kurup donanım hızlandırmalı 2D ve 3D vektörel grafikleri çizen, dünyadaki en köklü ve en yaygın **Çapraz Platform (Cross-Platform) Grafik API'sidir**. (Bir programlama dili değil, C/C++ ile çağırılan bir donanım köprüsü / Kütüphanesidir).

## Nedir ve Ne İşe Yarar?
1990'ların başında, 3 Boyutlu bir oyun veya CAD (Tasarım) programı yazıyorsanız, ekran kartına (Örn: 3Dfx Voodoo ya da S3 Graphics) piksel çizdirmek için O ekran kartının *kendi özel ve garip* Assembly/C kablolarını bilmeniz gerekiyordu. Oyununuz bir kartta çalışırken diğerinde çöküyordu.

Silicon Graphics dedi ki: "Bütün Ekran Kartı üreticilerine ve Yazılımcılara Ortak Bir Sözlük yazalım. Adı OpenGL olsun".
Siz C++ ile `glBegin(GL_TRIANGLES); glVertex3f(0.0, 1.0, 0.0); glEnd();` dediğinizde, bilgisayardaki ekran kartı Nvidia da olsa, AMD de olsa, Intel de olsa o fonksiyonu anlıyor ve donanım hızlandırmasıyla (CPU'yu yormadan) monitöre o üçgeni milyarlarca renk opsiyonuyla boyuyordu.

**Ne İşe Yarar?**
* **Oyun Motorları ve 3D Yazılımlar:** Klasik Minecraft (Java sürümü), Counter-Strike 1.6, Blender 3D, Maya ve eski Unity oyunlarının altında (Render makinesi olarak) mutlak suretle OpenGL yatar. Windows'a özel olan DirectX'in tarihteki (ve günümüzdeki) en büyük Evrensel (Linux/Mac/Windows) rakibi OpenGL'dir.
* **Mobil Oyunculuk (OpenGL ES):** Cep telefonlarındaki (Android/iOS) Milyarlarca mobil oyunun ekrana çizilmesini sağlayan şey, OpenGL'in telefonlar için küçültülmüş pürüzsüz hali olan **OpenGL ES** API'sidir. Bütün Angry Birds ve PUBG Mobile evreni bu kütüphaneyle boyanır.

## Dilin Mantığı ve Kod Yapısı
OpenGL'in kendisi C dilinde yazılmış devasa bir **State Machine (Durum Makinesi)** dir. Siz bir renk (Örn: Kırmızı) ayarlarsanız, siz onu "Mavi" ile değiştirene kadar Çizilen BÜTÜN modeller o anki Durum'a (Kırmızıya) boyanır. 

Modern OpenGL (v3.3 ve Sonrası), Eski (Legacy) "glBegin" komutlarını çöpe atmış; Onun yerine **Shader (GLSL - Grafik Dili)** mantığını getirmiştir. Bütün 3D Koordinat Noktaları (Vertexler) RAM'den (VBO) alınır, Ekran kartının VRAM'ine fırlatılır ve Vertex/Fragment Shaderlar ile boyanır.

**Örnek İşleyiş:**
OpenGL'in kalbi Matris (Matrix) matematiğidir. Kamerayı kaydırmak bile, Kameranın içindeki bütün dünyayı Ters Matris Çarpımıyla Geriye ittirmek demektir.

### Örnek Bir (Eski Mimarisiyle Başlayan) OpenGL Akışı: C++ ile 3D Üçgen Çizimi
*(Aşağıdaki kod C++ dilinde yazılmıştır, OpenGL kütüphanesine Emir verir)*

```cpp
/* OPENGL'IN LEGACY (ESKİ) AMA EN ANLAŞILIR ILK VERSIYONU (State Machine Mantigi) */

#include <GL/glut.h> // Pencere ve OpenGL baglantisini kuran arac kutuphanesi

// Ekran Her Cizileceginde (Saniyede 60 Kere= 60 FPS) Bu fonksiyon Cagirilir:
void ekranCiz(void) {
    
    // 1. EKRANI TEMİZLE (Eski Frameden(Kareden) kalan copleri boya)
    // Siyah (0,0,0) Arka plani sil
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    // 2. OPENGL DURUMUNU (STATE) DEGISTIR: Fircayi KIRMIZI (R=1, G=0, B=0) Rengine bandir!
    glColor3f(1.0f, 0.0f, 0.0f);
    
    // 3. EKRANA CIZIM BASLAT: BEN BIR UCGEN(TRIANGLE) Cizecegim diyoruz!
    glBegin(GL_TRIANGLES);
        
        // Ucgenin Birinci Kosesi (UST NOKTA - X,Y,Z kordinatlari 3D)
        glVertex3f( 0.0f,  1.0f, 0.0f);
        
        // Fircamizin Rengini Simdi Yesile (R=0, G=1, B=0) daldik! (Ucgenin Sol Altı Yesil Olacak)
        glColor3f(0.0f, 1.0f, 0.0f);
        glVertex3f(-1.0f, -1.0f, 0.0f); // SOL ALT Kose
        
        // Fircamizi Simdi Maviye Daldik!
        glColor3f(0.0f, 0.0f, 1.0f);
        glVertex3f( 1.0f, -1.0f, 0.0f); // SAG ALT Kose
        
    glEnd(); // Cizimi Bitir! (GPU Uçgenin ortasini Gökkuşağı gibi Karıştırarak(Interpolate) Çizer!)
    
    // 4. CIzdigin Sahneyi EKRANA BAS (Monitoru Guncelle - Swap Buffers)
    glutSwapBuffers();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    
    glutInitWindowSize(800, 600); // 800x600 Pencere Ac
    glutCreateWindow("Ilk 3D Ucgenim - OpenGL");
    
    // Sonsuz Donguyu Baslat (Oyun Motoru Dongusu - Game Loop)
    glutDisplayFunc(ekranCiz); 
    glutMainLoop();
    return 0;
}
```
Yukarıdaki C++ kodunda görülen ve BAŞ harfleri `gl` ile başlayan (`glClear`, `glVertex3f`) her kelime, doğrudan Ekran Kartı Sürücünüze (Nvidia/Amd) giden Evrensel OpenGL API Emridir.

## Kimler Kullanır?
* Evrendeki oyun motorlarının (Unreal Engine, Unity, Godot) **C++ Motor (Engine/Rendering) Programcıları**. (Siz Godot veya Unity'de C# ile "Zıpla" yazarsınız, o motorun arkaplanındaki C++ Çekirdeği bu zıplamayı Ekrana Yansıtmak için Milisaniyeler içinde Yukarıdaki gibi OpenGL/Vulkan GL kodları koşturur).
* Üniversitelerde (Computer Graphics) Bilgisayar Grafikleri, Cebir ve Işıklandırma/Fizik Motoru Simülasyonu çalışan Bilgisayar Mühendisliği Öğrencileri (Kesinlikle öğrenmek zorunda oldukları mutlak çekirdektir).
* **Günümüzde:** OpenGL eski bir mimari olduğu için, Apple sisteminde (Metal), Microsoft'ta (DirectX 12) ve Açık Kaynakta ise **Vulkan API** denen (OpenGL'in çok daha düşük seviyeli ve Işık hızındaki Torunu) yeni bir mimariye yerini yavaş yavaş devretmektedir.
